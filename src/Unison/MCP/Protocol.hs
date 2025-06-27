{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Unison.MCP.Protocol
  ( -- * Core Protocol Types
    Request(..)
  , Response(..)
  , Method(..)
  , MCPError(..)
  , ErrorCode(..)
  
    -- * MCP Specific Types
  , InitializeParams(..)
  , InitializeResult(..)
  , ServerInfo(..)
  , ClientInfo(..)
  , Tool(..)
  , ToolCall(..)
  , ToolResult(..)
  , Resource(..)
  , Prompt(..)
  
    -- * JSON-RPC Types
  , RequestId(..)
  , encodeRequest
  , encodeResponse
  , decodeRequest
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Text (Text)
import GHC.Generics

-- | JSON-RPC Request ID
data RequestId = IdInt Int | IdString Text
  deriving (Show, Eq, Generic)

instance ToJSON RequestId where
  toJSON (IdInt i) = toJSON i
  toJSON (IdString s) = toJSON s

instance FromJSON RequestId where
  parseJSON v = (IdInt <$> parseJSON v) <|> (IdString <$> parseJSON v)

-- | MCP Methods
data Method
  = Initialize
  | ToolsList
  | ToolsCall
  | ResourcesList
  | ResourcesRead
  | PromptsList
  | PromptsGet
  | Ping
  | Unknown Text
  deriving (Show, Eq)

instance ToJSON Method where
  toJSON Initialize = String "initialize"
  toJSON ToolsList = String "tools/list"
  toJSON ToolsCall = String "tools/call"
  toJSON ResourcesList = String "resources/list"
  toJSON ResourcesRead = String "resources/read"
  toJSON PromptsList = String "prompts/list"
  toJSON PromptsGet = String "prompts/get"
  toJSON Ping = String "ping"
  toJSON (Unknown m) = String m

instance FromJSON Method where
  parseJSON = withText "Method" $ \case
    "initialize" -> pure Initialize
    "tools/list" -> pure ToolsList
    "tools/call" -> pure ToolsCall
    "resources/list" -> pure ResourcesList
    "resources/read" -> pure ResourcesRead
    "prompts/list" -> pure PromptsList
    "prompts/get" -> pure PromptsGet
    "ping" -> pure Ping
    other -> pure (Unknown other)

-- | JSON-RPC Request
data Request = Request
  { requestJsonrpc :: Text
  , requestId :: RequestId
  , requestMethod :: Method
  , requestParams :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON Request where
  toJSON Request{..} = object
    [ "jsonrpc" .= requestJsonrpc
    , "id" .= requestId
    , "method" .= requestMethod
    , "params" .= requestParams
    ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o -> Request
    <$> o .: "jsonrpc"
    <*> o .: "id"
    <*> o .: "method"
    <*> o .:? "params"

-- | Error codes
data ErrorCode
  = ParseError
  | InvalidRequest
  | MethodNotFound
  | InvalidParams
  | InternalError
  | ResourceNotFound
  | ResourceError
  deriving (Show, Eq)

errorCodeToInt :: ErrorCode -> Int
errorCodeToInt ParseError = -32700
errorCodeToInt InvalidRequest = -32600
errorCodeToInt MethodNotFound = -32601
errorCodeToInt InvalidParams = -32602
errorCodeToInt InternalError = -32603
errorCodeToInt ResourceNotFound = -32002
errorCodeToInt ResourceError = -32003

-- | JSON-RPC Error
data MCPError = MCPError
  { errorCode :: ErrorCode
  , errorMessage :: Text
  , errorData :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON MCPError where
  toJSON MCPError{..} = object
    [ "code" .= errorCodeToInt errorCode
    , "message" .= errorMessage
    , "data" .= errorData
    ]

instance FromJSON MCPError where
  parseJSON = withObject "MCPError" $ \o -> MCPError
    <$> (intToErrorCode <$> o .: "code")
    <*> o .: "message"
    <*> o .:? "data"
    where
      intToErrorCode :: Int -> ErrorCode
      intToErrorCode (-32700) = ParseError
      intToErrorCode (-32600) = InvalidRequest
      intToErrorCode (-32601) = MethodNotFound
      intToErrorCode (-32602) = InvalidParams
      intToErrorCode (-32603) = InternalError
      intToErrorCode (-32002) = ResourceNotFound
      intToErrorCode (-32003) = ResourceError
      intToErrorCode _ = InternalError

-- | JSON-RPC Response
data Response = Response
  { responseJsonrpc :: Text
  , responseId :: Maybe RequestId
  , responseResult :: Maybe Value
  , responseError :: Maybe MCPError
  } deriving (Show, Eq, Generic)

instance ToJSON Response where
  toJSON Response{..} = object $ filter ((/= Null) . snd)
    [ "jsonrpc" .= responseJsonrpc
    , "id" .= responseId
    , "result" .= responseResult
    , "error" .= responseError
    ]

instance FromJSON Response where
  parseJSON = withObject "Response" $ \o -> Response
    <$> o .: "jsonrpc"
    <*> o .:? "id"
    <*> o .:? "result"
    <*> o .:? "error"

-- | Client information
data ClientInfo = ClientInfo
  { clientName :: Text
  , clientVersion :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ClientInfo where
  toJSON ClientInfo{..} = object
    [ "name" .= clientName
    , "version" .= clientVersion
    ]

instance FromJSON ClientInfo where
  parseJSON = withObject "ClientInfo" $ \o -> ClientInfo
    <$> o .: "name"
    <*> o .: "version"

-- | Server information
data ServerInfo = ServerInfo
  { serverName :: Text
  , serverVersion :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ServerInfo where
  toJSON ServerInfo{..} = object
    [ "name" .= serverName
    , "version" .= serverVersion
    ]

instance FromJSON ServerInfo where
  parseJSON = withObject "ServerInfo" $ \o -> ServerInfo
    <$> o .: "name"
    <*> o .: "version"

-- | Initialize parameters
data InitializeParams = InitializeParams
  { initParamsProtocolVersion :: Text
  , initParamsCapabilities :: KeyMap Value
  , initParamsClientInfo :: ClientInfo
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeParams where
  toJSON InitializeParams{..} = object
    [ "protocolVersion" .= initParamsProtocolVersion
    , "capabilities" .= initParamsCapabilities
    , "clientInfo" .= initParamsClientInfo
    ]

instance FromJSON InitializeParams where
  parseJSON = withObject "InitializeParams" $ \o -> InitializeParams
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .: "clientInfo"

-- | Initialize result
data InitializeResult = InitializeResult
  { initResultProtocolVersion :: Text
  , initResultCapabilities :: KeyMap Value
  , initResultServerInfo :: ServerInfo
  } deriving (Show, Eq, Generic)

instance ToJSON InitializeResult where
  toJSON InitializeResult{..} = object
    [ "protocolVersion" .= initResultProtocolVersion
    , "capabilities" .= initResultCapabilities
    , "serverInfo" .= initResultServerInfo
    ]

instance FromJSON InitializeResult where
  parseJSON = withObject "InitializeResult" $ \o -> InitializeResult
    <$> o .: "protocolVersion"
    <*> o .: "capabilities"
    <*> o .: "serverInfo"

-- | Tool definition
data Tool = Tool
  { toolName :: Text
  , toolDescription :: Maybe Text
  , toolInputSchema :: Value
  } deriving (Show, Eq, Generic)

instance ToJSON Tool where
  toJSON Tool{..} = object
    [ "name" .= toolName
    , "description" .= toolDescription
    , "inputSchema" .= toolInputSchema
    ]

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o -> Tool
    <$> o .: "name"
    <*> o .:? "description"
    <*> o .: "inputSchema"

-- | Tool call request
data ToolCall = ToolCall
  { toolCallName :: Text
  , toolCallArguments :: Maybe Value
  } deriving (Show, Eq, Generic)

instance ToJSON ToolCall where
  toJSON ToolCall{..} = object
    [ "name" .= toolCallName
    , "arguments" .= toolCallArguments
    ]

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \o -> ToolCall
    <$> o .: "name"
    <*> o .:? "arguments"

-- | Tool call result
data ToolResult = ToolResult
  { toolResultContent :: [Value]
  , toolResultIsError :: Maybe Bool
  } deriving (Show, Eq, Generic)

instance ToJSON ToolResult where
  toJSON ToolResult{..} = object
    [ "content" .= toolResultContent
    , "isError" .= toolResultIsError
    ]

-- | Resource definition
data Resource = Resource
  { resourceUri :: Text
  , resourceName :: Text
  , resourceDescription :: Maybe Text
  , resourceMimeType :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON Resource where
  toJSON Resource{..} = object
    [ "uri" .= resourceUri
    , "name" .= resourceName
    , "description" .= resourceDescription
    , "mimeType" .= resourceMimeType
    ]

-- | Prompt definition
data Prompt = Prompt
  { promptName :: Text
  , promptDescription :: Maybe Text
  , promptArguments :: Maybe [Value]
  } deriving (Show, Eq, Generic)

instance ToJSON Prompt where
  toJSON Prompt{..} = object
    [ "name" .= promptName
    , "description" .= promptDescription
    , "arguments" .= promptArguments
    ]

-- | Helper functions
encodeRequest :: Request -> Value
encodeRequest = toJSON

encodeResponse :: Response -> Value
encodeResponse = toJSON

decodeRequest :: Value -> Either String Request
decodeRequest v = case fromJSON v of
  Success r -> Right r
  Data.Aeson.Error e -> Left e