{-# LANGUAGE OverloadedStrings #-}

module IntegrationSpec where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import System.Directory
import System.FilePath
import System.IO
import System.Process
import System.IO.Temp

import Unison.MCP.Protocol

spec :: Spec
spec = describe "MCP Server Integration Tests" $ do
  it "initializes and responds to basic requests" $ do
    withTempDirectory "." "test-codebase-" $ \tmpDir -> do
      -- Create a test codebase
      callProcess "ucm" ["--codebase-create", tmpDir]
      
      -- Start the server
      withMCPServer tmpDir $ \(stdin, stdout) -> do
        -- Test initialize
        let initReq = Request
              { requestJsonrpc = "2.0"
              , requestId = IdInt 1
              , requestMethod = Initialize
              , requestParams = Just $ toJSON $ InitializeParams
                  { initParamsProtocolVersion = "2024-11-05"
                  , initParamsCapabilities = mempty
                  , initParamsClientInfo = ClientInfo "test" "1.0"
                  }
              }
        
        response <- sendAndReceive stdin stdout initReq
        response `shouldSatisfy` isSuccessResponse
        
        -- Test tools/list
        let toolsReq = Request
              { requestJsonrpc = "2.0"
              , requestId = IdInt 2
              , requestMethod = ToolsList
              , requestParams = Nothing
              }
        
        toolsResponse <- sendAndReceive stdin stdout toolsReq
        toolsResponse `shouldSatisfy` isSuccessResponse

  it "adds and runs Unison code" $ do
    withTempDirectory "." "test-codebase-" $ \tmpDir -> do
      callProcess "ucm" ["--codebase-create", tmpDir]
      
      withMCPServer tmpDir $ \(stdin, stdout) -> do
        -- Initialize first
        initialize stdin stdout
        
        -- Add code
        let addReq = Request
              { requestJsonrpc = "2.0"
              , requestId = IdInt 2
              , requestMethod = ToolsCall
              , requestParams = Just $ toJSON $ ToolCall
                  { toolCallName = "ucm_add"
                  , toolCallArguments = Just $ object 
                      ["code" .= String "double : Nat -> Nat\ndouble x = x * 2"]
                  }
              }
        
        addResponse <- sendAndReceive stdin stdout addReq
        addResponse `shouldSatisfy` isSuccessResponse
        
        -- Wait for UCM to process
        threadDelay 1000000  -- 1 second
        
        -- Run code
        let runReq = Request
              { requestJsonrpc = "2.0"
              , requestId = IdInt 3
              , requestMethod = ToolsCall
              , requestParams = Just $ toJSON $ ToolCall
                  { toolCallName = "ucm_run"
                  , toolCallArguments = Just $ object 
                      ["expression" .= String "double 21"]
                  }
              }
        
        runResponse <- sendAndReceive stdin stdout runReq
        runResponse `shouldSatisfy` isSuccessResponse

  it "handles invalid requests gracefully" $ do
    withTempDirectory "." "test-codebase-" $ \tmpDir -> do
      callProcess "ucm" ["--codebase-create", tmpDir]
      
      withMCPServer tmpDir $ \(stdin, stdout) -> do
        -- Send request without initializing
        let req = Request
              { requestJsonrpc = "2.0"
              , requestId = IdInt 1
              , requestMethod = ToolsList
              , requestParams = Nothing
              }
        
        response <- sendAndReceive stdin stdout req
        response `shouldSatisfy` isErrorResponse

-- | Start MCP server and provide handles
withMCPServer :: FilePath -> ((Handle, Handle) -> IO a) -> IO a
withMCPServer codebasePath action = do
  -- Get the path to the executable
  let serverPath = ".stack-work/dist/x86_64-linux-tinfo6/ghc-9.6.3/build/unison-mcp-server/unison-mcp-server"
  
  -- Start the server
  (Just stdin, Just stdout, Just stderr, ph) <- createProcess 
    (proc serverPath [codebasePath])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  
  -- Set binary mode
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  -- Run action
  result <- action (stdin, stdout)
  
  -- Cleanup
  hClose stdin
  hClose stdout
  hClose stderr
  terminateProcess ph
  
  return result

-- | Send request and receive response
sendAndReceive :: Handle -> Handle -> Request -> IO Response
sendAndReceive stdin stdout req = do
  -- Send request
  BS8.hPutStrLn stdin $ LBS.toStrict $ encode req
  hFlush stdin
  
  -- Read response
  line <- BS8.hGetLine stdout
  case eitherDecode (LBS.fromStrict line) of
    Left err -> error $ "Failed to decode response: " ++ err
    Right resp -> return resp

-- | Initialize the server
initialize :: Handle -> Handle -> IO ()
initialize stdin stdout = do
  let req = Request
        { requestJsonrpc = "2.0"
        , requestId = IdInt 1
        , requestMethod = Initialize
        , requestParams = Just $ toJSON $ InitializeParams
            { initParamsProtocolVersion = "2024-11-05"
            , initParamsCapabilities = mempty
            , initParamsClientInfo = ClientInfo "test" "1.0"
            }
        }
  _ <- sendAndReceive stdin stdout req
  return ()

-- | Check if response is successful
isSuccessResponse :: Response -> Bool
isSuccessResponse Response{..} = 
  responseError == Nothing && responseResult /= Nothing

-- | Check if response is an error
isErrorResponse :: Response -> Bool
isErrorResponse Response{..} = 
  responseError /= Nothing