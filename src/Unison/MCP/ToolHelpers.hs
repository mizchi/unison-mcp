{-# LANGUAGE OverloadedStrings #-}

module Unison.MCP.ToolHelpers
  ( -- * Tool Definition Helpers
    simpleTool
  , toolWithStringParam
  , toolWithOptionalStringParam
  , toolWithNumberParam
  , toolWithRequiredParams
    -- * Schema Builders
  , stringProperty
  , numberProperty
  , booleanProperty
  , objectSchema
  , emptySchema
    -- * Result Helpers
  , textResult
  , errorResult
  , successResult
  , textContent
    -- * Parameter Extraction
  , getStringParam
  , getOptionalStringParam
  , getNumberParam
  , getOptionalNumberParam
  ) where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.KeyMap (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Unison.MCP.Protocol

-- | Create a simple tool with no parameters
simpleTool :: Text -> Text -> Tool
simpleTool name desc = Tool
  { toolName = name
  , toolDescription = Just desc
  , toolInputSchema = emptySchema
  }

-- | Create a tool with a single required string parameter
toolWithStringParam :: Text -> Text -> Text -> Text -> Tool
toolWithStringParam name desc paramName paramDesc = Tool
  { toolName = name
  , toolDescription = Just desc
  , toolInputSchema = objectSchema
      [(paramName, stringProperty paramDesc)]
      [paramName]
  }

-- | Create a tool with a single optional string parameter
toolWithOptionalStringParam :: Text -> Text -> Text -> Text -> Tool
toolWithOptionalStringParam name desc paramName paramDesc = Tool
  { toolName = name
  , toolDescription = Just desc
  , toolInputSchema = objectSchema
      [(paramName, stringProperty paramDesc)]
      []
  }

-- | Create a tool with a single optional number parameter
toolWithNumberParam :: Text -> Text -> Text -> Text -> Tool
toolWithNumberParam name desc paramName paramDesc = Tool
  { toolName = name
  , toolDescription = Just desc
  , toolInputSchema = objectSchema
      [(paramName, numberProperty paramDesc)]
      []
  }

-- | Create a tool with multiple parameters
toolWithRequiredParams :: Text -> Text -> [(Text, Value)] -> [Text] -> Tool
toolWithRequiredParams name desc props required = Tool
  { toolName = name
  , toolDescription = Just desc
  , toolInputSchema = objectSchema props required
  }

-- | Create an empty object schema
emptySchema :: Value
emptySchema = object
  [ "type" .= String "object"
  , "properties" .= object []
  ]

-- | Create an object schema with properties
objectSchema :: [(Text, Value)] -> [Text] -> Value
objectSchema props required = object $
  [ "type" .= String "object"
  , "properties" .= object (map (\(k, v) -> (fromText k, v)) props)
  ] ++ if null required
       then []
       else ["required" .= required]

-- | Create a string property schema
stringProperty :: Text -> Value
stringProperty desc = object
  [ "type" .= String "string"
  , "description" .= String desc
  ]

-- | Create a number property schema
numberProperty :: Text -> Value
numberProperty desc = object
  [ "type" .= String "number"
  , "description" .= String desc
  ]

-- | Create a boolean property schema
booleanProperty :: Text -> Value
booleanProperty desc = object
  [ "type" .= String "boolean"
  , "description" .= String desc
  ]

-- | Create a successful text result
textResult :: Text -> ToolResult
textResult text = ToolResult
  { toolResultContent = [textContent text]
  , toolResultIsError = Just False
  }

-- | Create a successful result
successResult :: Text -> ToolResult
successResult = textResult

-- | Helper to create error results
errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { toolResultContent = [textContent msg]
  , toolResultIsError = Just True
  }

-- | Helper to create text content block
textContent :: Text -> Value
textContent text = object ["type" .= String "text", "text" .= text]

-- | Extract a string parameter from arguments
getStringParam :: Text -> Maybe Value -> Maybe Text
getStringParam paramName (Just (Object o)) = 
  case lookup (fromText paramName) (toList o) of
    Just (String s) -> Just s
    _ -> Nothing
getStringParam _ _ = Nothing

-- | Extract an optional string parameter with default
getOptionalStringParam :: Text -> Text -> Maybe Value -> Text
getOptionalStringParam paramName defaultValue params =
  case getStringParam paramName params of
    Just s -> s
    Nothing -> defaultValue

-- | Extract a number parameter
getNumberParam :: Text -> Maybe Value -> Maybe Double
getNumberParam paramName (Just (Object o)) =
  case lookup (fromText paramName) (toList o) of
    Just (Number n) -> Just (realToFrac n)
    _ -> Nothing
getNumberParam _ _ = Nothing

-- | Extract an optional number parameter with default
getOptionalNumberParam :: Text -> Double -> Maybe Value -> Double
getOptionalNumberParam paramName defaultValue params =
  case getNumberParam paramName params of
    Just n -> n
    Nothing -> defaultValue