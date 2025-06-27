{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Unison.MCP.Server
  ( runMCPServer
  , MCPConfig(..)
  ) where

import Control.Concurrent.STM
import Control.Exception (catch, SomeException, finally)
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.KeyMap (singleton)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import System.IO

import Unison.MCP.Protocol
import Unison.MCP.Tools
import Unison.MCP.UCM (UCMHandle)
import qualified Unison.MCP.UCM as UCM

-- | MCP Server configuration
data MCPConfig = MCPConfig
  { configCodebasePath :: FilePath
  , configServerName :: Text
  , configServerVersion :: Text
  }

-- | Server state
data ServerState = ServerState
  { stateConfig :: MCPConfig
  , stateInitialized :: TVar Bool
  , stateUCM :: TVar (Maybe UCMHandle)
  }

-- | Run the MCP server on stdio
runMCPServer :: MCPConfig -> IO ()
runMCPServer config = do
  -- Set up stdio for binary mode
  hSetBinaryMode stdin True
  hSetBinaryMode stdout True
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  
  -- Initialize server state
  initializedVar <- newTVarIO False
  ucmVar <- newTVarIO Nothing
  let state = ServerState
        { stateConfig = config
        , stateInitialized = initializedVar
        , stateUCM = ucmVar
        }
  
  -- Start the server loop without initializing UCM
  serverLoop state `finally` cleanup state
  where
    cleanup :: ServerState -> IO ()
    cleanup state = do
      maybeUCM <- readTVarIO (stateUCM state)
      case maybeUCM of
        Just ucm -> UCM.stopUCM ucm
        Nothing -> return ()

-- | Main server loop
serverLoop :: ServerState -> IO ()
serverLoop state = forever $ do
  -- Read a line from stdin
  lineBS <- BS8.hGetLine stdin
  let line = LBS.fromStrict lineBS
  
  -- Parse as JSON-RPC request
  case eitherDecode line of
    Left err -> do
      -- Send parse error
      let response = Response
            { responseJsonrpc = "2.0"
            , responseId = Nothing
            , responseResult = Nothing
            , responseError = Just $ MCPError
                { errorCode = ParseError
                , errorMessage = T.pack err
                , errorData = Nothing
                }
            }
      sendResponse response
    
    Right request -> do
      -- Handle the request
      response <- handleRequest state request
      sendResponse response

-- | Send a response to stdout
sendResponse :: Response -> IO ()
sendResponse response = do
  LBS8.putStrLn $ encode response
  hFlush stdout

-- | Handle a JSON-RPC request
handleRequest :: ServerState -> Request -> IO Response
handleRequest state request@Request{..} = do
  initialized <- readTVarIO (stateInitialized state)
  
  -- Check if initialization is required
  if (not initialized && requestMethod /= Initialize)
    then return Response
      { responseJsonrpc = "2.0"
      , responseId = Just requestId
      , responseResult = Nothing
      , responseError = Just $ MCPError
          { errorCode = InvalidRequest
          , errorMessage = "Server not initialized"
          , errorData = Nothing
          }
      }
    else do
      -- Route to appropriate handler
      result <- case requestMethod of
        Initialize -> handleInitialize state requestParams
        ToolsList -> handleToolsList state
        ToolsCall -> handleToolsCall state requestParams
        Ping -> return $ Right $ String "pong"
        _ -> return $ Left $ MCPError
          { errorCode = MethodNotFound
          , errorMessage = "Method not found"
          , errorData = Nothing
          }
      
      -- Build response
      return $ case result of
        Left err -> Response
          { responseJsonrpc = "2.0"
          , responseId = Just requestId
          , responseResult = Nothing
          , responseError = Just err
          }
        Right res -> Response
          { responseJsonrpc = "2.0"
          , responseId = Just requestId
          , responseResult = Just res
          , responseError = Nothing
          }

-- | Initialize UCM process
initializeUCM :: ServerState -> IO (Either MCPError ())
initializeUCM state = do
  let codebasePath = configCodebasePath (stateConfig state)
  result <- catch (Right <$> UCM.startUCM codebasePath) $ \(e :: SomeException) ->
    return $ Left $ MCPError
      { errorCode = InternalError
      , errorMessage = T.pack $ "Failed to start UCM: " ++ show e
      , errorData = Nothing
      }
  
  case result of
    Right ucm -> do
      atomically $ writeTVar (stateUCM state) (Just ucm)
      return $ Right ()
    Left err -> return $ Left err

-- | Handle initialize request
handleInitialize :: ServerState -> Maybe Value -> IO (Either MCPError Value)
handleInitialize state (Just params) = do
  case fromJSON params :: Result InitializeParams of
    Success initParams -> do
      -- Initialize UCM if not already initialized
      maybeUCM <- readTVarIO (stateUCM state)
      case maybeUCM of
        Nothing -> do
          -- Start UCM process
          result <- initializeUCM state
          case result of
            Left err -> return $ Left err
            Right () -> continueInitialization state
        Just _ -> continueInitialization state
    
    Data.Aeson.Error err -> return $ Left $ MCPError
      { errorCode = InvalidParams
      , errorMessage = T.pack err
      , errorData = Nothing
      }
  where
    continueInitialization :: ServerState -> IO (Either MCPError Value)
    continueInitialization state = do
      -- Mark as initialized
      atomically $ writeTVar (stateInitialized state) True
      
      -- Return initialization result
      let result = InitializeResult
            { initResultProtocolVersion = "2024-11-05"
            , initResultCapabilities = singleton "tools" (object [])
            , initResultServerInfo = ServerInfo
                { serverName = configServerName (stateConfig state)
                , serverVersion = configServerVersion (stateConfig state)
                }
            }
      return $ Right $ toJSON result

handleInitialize _ Nothing = return $ Left $ MCPError
  { errorCode = InvalidParams
  , errorMessage = "Missing initialization parameters"
  , errorData = Nothing
  }

-- | Handle tools/list request
handleToolsList :: ServerState -> IO (Either MCPError Value)
handleToolsList _ = do
  let result = object [ "tools" .= availableTools ]
  return $ Right result

-- | Handle tools/call request
handleToolsCall :: ServerState -> Maybe Value -> IO (Either MCPError Value)
handleToolsCall state (Just params) = do
  maybeUCM <- readTVarIO (stateUCM state)
  case (fromJSON params :: Result ToolCall, maybeUCM) of
    (Success toolCall, Just ucm) -> do
      result <- handleToolCall ucm toolCall
      return $ Right $ toJSON result
    
    (Data.Aeson.Error err, _) -> return $ Left $ MCPError
      { errorCode = InvalidParams
      , errorMessage = T.pack err
      , errorData = Nothing
      }
    
    (_, Nothing) -> return $ Left $ MCPError
      { errorCode = InternalError
      , errorMessage = "UCM not available"
      , errorData = Nothing
      }

handleToolsCall _ Nothing = return $ Left $ MCPError
  { errorCode = InvalidParams
  , errorMessage = "Missing tool call parameters"
  , errorData = Nothing
  }