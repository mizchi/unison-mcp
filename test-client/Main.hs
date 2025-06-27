{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception (IOException, catch)
import Control.Monad (forM_, when, forever)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import System.Process

import Unison.MCP.Protocol
import Data.Aeson.KeyMap (toList)

-- | Test client configuration
data TestConfig = TestConfig
  { serverPath :: FilePath
  , codebasePath :: FilePath
  , verbose :: Bool
  }

-- | Test result
data TestResult = TestResult
  { testName :: Text
  , testPassed :: Bool
  , testMessage :: Text
  } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [serverPath, codebasePath] -> runTests $ TestConfig serverPath codebasePath False
    [serverPath, codebasePath, "--verbose"] -> runTests $ TestConfig serverPath codebasePath True
    _ -> do
      hPutStrLn stderr "Usage: test-client <server-path> <codebase-path> [--verbose]"
      exitFailure

runTests :: TestConfig -> IO ()
runTests config = do
  putStrLn "Starting MCP test client..."
  
  -- Start the MCP server
  (Just stdin, Just stdout, Just stderr, ph) <- createProcess (proc (serverPath config) [codebasePath config])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  
  -- Set handles to binary mode
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  -- Read and discard any stderr output from server startup
  _ <- forkIO $ forever $ do
    line <- hGetLine stderr `catch` \(_ :: IOException) -> return ""
    when (verbose config) $ putStrLn $ "[Server stderr]: " ++ line
  
  -- Wait for server to be ready
  threadDelay 3000000  -- 3 seconds
  
  -- Run test suite
  results <- sequence
    [ testInitialize stdin stdout config
    , testToolsList stdin stdout config
    , testUcmFind stdin stdout config
    , testUcmAdd stdin stdout config
    , testUcmRun stdin stdout config
    , testUcmView stdin stdout config
    ]
  
  -- Print results
  putStrLn "\n=== Test Results ==="
  forM_ results $ \TestResult{..} -> do
    let status = if testPassed then "✓" else "✗"
    putStrLn $ T.unpack $ status <> " " <> testName <> ": " <> testMessage
  
  -- Cleanup
  hClose stdin
  hClose stdout
  hClose stderr
  terminateProcess ph
  
  -- Exit with appropriate code
  let allPassed = all testPassed results
  if allPassed
    then exitSuccess
    else exitFailure

-- | Send request and receive response
sendRequest :: Handle -> Handle -> TestConfig -> Request -> IO (Either Text Response)
sendRequest stdin stdout config req = do
  when (verbose config) $ do
    putStrLn $ "Sending: " ++ show (encode req)
  
  -- Send request
  BS8.hPutStrLn stdin $ LBS8.toStrict $ encode req
  hFlush stdin
  
  -- Read response
  line <- BS8.hGetLine stdout
  when (verbose config) $ do
    putStrLn $ "Received: " ++ BS8.unpack line
  
  case eitherDecode (LBS8.fromStrict line) of
    Left err -> return $ Left $ T.pack err
    Right resp -> return $ Right resp

-- | Test initialize method
testInitialize :: Handle -> Handle -> TestConfig -> IO TestResult
testInitialize stdin stdout config = do
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 1
        , requestMethod = Initialize
        , requestParams = Just $ toJSON $ InitializeParams
            { initParamsProtocolVersion = "2024-11-05"
            , initParamsCapabilities = mempty
            , initParamsClientInfo = ClientInfo 
                { clientName = "test-client"
                , clientVersion = "0.1.0"
                }
            }
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "Initialize" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "Initialize" False (errorMessage err)
      Nothing -> return $ TestResult "Initialize" True "Server initialized successfully"

-- | Test tools/list method
testToolsList :: Handle -> Handle -> TestConfig -> IO TestResult
testToolsList stdin stdout config = do
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 2
        , requestMethod = ToolsList
        , requestParams = Nothing
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "Tools List" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "Tools List" False (errorMessage err)
      Nothing -> case responseResult of
        Just (Object o) -> case lookup "tools" (map (\(k,v) -> (T.pack (show k), v)) $ toList o) of
          Just (Array tools) -> 
            return $ TestResult "Tools List" True $ 
              "Found " <> T.pack (show $ length tools) <> " tools"
          _ -> return $ TestResult "Tools List" False "Invalid tools format"
        _ -> return $ TestResult "Tools List" False "Invalid response format"

-- | Test ucm_find tool
testUcmFind :: Handle -> Handle -> TestConfig -> IO TestResult
testUcmFind stdin stdout config = do
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 3
        , requestMethod = ToolsCall
        , requestParams = Just $ toJSON $ ToolCall
            { toolCallName = "ucm_find"
            , toolCallArguments = Just $ object ["query" .= String "test"]
            }
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "UCM Find" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "UCM Find" False (errorMessage err)
      Nothing -> return $ TestResult "UCM Find" True "Search completed"

-- | Test ucm_add tool
testUcmAdd :: Handle -> Handle -> TestConfig -> IO TestResult
testUcmAdd stdin stdout config = do
  let testCode = "testFunction : Nat -> Nat\ntestFunction x = x + 1"
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 4
        , requestMethod = ToolsCall
        , requestParams = Just $ toJSON $ ToolCall
            { toolCallName = "ucm_add"
            , toolCallArguments = Just $ object ["code" .= String testCode]
            }
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "UCM Add" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "UCM Add" False (errorMessage err)
      Nothing -> return $ TestResult "UCM Add" True "Code added successfully"

-- | Test ucm_run tool
testUcmRun :: Handle -> Handle -> TestConfig -> IO TestResult
testUcmRun stdin stdout config = do
  -- Wait a bit for previous add to complete
  threadDelay 500000  -- 0.5 seconds
  
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 5
        , requestMethod = ToolsCall
        , requestParams = Just $ toJSON $ ToolCall
            { toolCallName = "ucm_run"
            , toolCallArguments = Just $ object ["expression" .= String "testFunction 5"]
            }
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "UCM Run" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "UCM Run" False (errorMessage err)
      Nothing -> return $ TestResult "UCM Run" True "Expression evaluated"

-- | Test ucm_view tool
testUcmView :: Handle -> Handle -> TestConfig -> IO TestResult
testUcmView stdin stdout config = do
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 6
        , requestMethod = ToolsCall
        , requestParams = Just $ toJSON $ ToolCall
            { toolCallName = "ucm_view"
            , toolCallArguments = Just $ object ["name" .= String "testFunction"]
            }
        }
  
  result <- sendRequest stdin stdout config req
  case result of
    Left err -> return $ TestResult "UCM View" False err
    Right Response{..} -> case responseError of
      Just err -> return $ TestResult "UCM View" False (errorMessage err)
      Nothing -> return $ TestResult "UCM View" True "Definition viewed"

