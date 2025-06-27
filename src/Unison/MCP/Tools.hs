{-# LANGUAGE OverloadedStrings #-}

module Unison.MCP.Tools
  ( availableTools
  , handleToolCall
  ) where

import Data.Aeson
import Data.Aeson.KeyMap (toList)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO, liftIO)

import Unison.MCP.Protocol
import Unison.MCP.UCM (UCMHandle)
import qualified Unison.MCP.UCM as UCM

-- | List of available tools
availableTools :: [Tool]
availableTools =
  [ Tool
      { toolName = "ucm_find"
      , toolDescription = Just "Search for definitions in the Unison codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "query" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Search query (name or type)"
                  ]
              ]
          , "required" .= ["query" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_add"
      , toolDescription = Just "Add definitions to the codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "code" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Unison code to add"
                  ]
              ]
          , "required" .= ["code" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_run"
      , toolDescription = Just "Execute a Unison expression"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "expression" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Unison expression to run"
                  ]
              ]
          , "required" .= ["expression" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_list_projects"
      , toolDescription = Just "List all projects in the codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "ucm_switch_project"
      , toolDescription = Just "Switch to a different project"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "project" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Project name to switch to"
                  ]
              ]
          , "required" .= ["project" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_list_branches"
      , toolDescription = Just "List branches in the current project"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "ucm_switch_branch"
      , toolDescription = Just "Switch to a different branch"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "branch" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Branch name to switch to"
                  ]
              ]
          , "required" .= ["branch" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_dependencies"
      , toolDescription = Just "Show dependencies of a definition"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Name of the definition"
                  ]
              ]
          , "required" .= ["name" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_view"
      , toolDescription = Just "View the source of a definition"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Name of the definition to view"
                  ]
              ]
          , "required" .= ["name" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_update"
      , toolDescription = Just "Update existing definitions in the codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "ucm_ls"
      , toolDescription = Just "List contents of a namespace"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "namespace" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Namespace to list (optional)"
                  ]
              ]
          ]
      }
  , Tool
      { toolName = "ucm_delete"
      , toolDescription = Just "Delete a definition from the codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Name of the definition to delete"
                  ]
              ]
          , "required" .= ["name" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_test"
      , toolDescription = Just "Run tests in the codebase"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "pattern" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Test pattern to match (optional)"
                  ]
              ]
          ]
      }
  , Tool
      { toolName = "ucm_project_create"
      , toolDescription = Just "Create a new project"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Name of the project to create"
                  ]
              ]
          , "required" .= ["name" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_branch_create"
      , toolDescription = Just "Create a new branch"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Name of the branch to create"
                  ]
              ]
          , "required" .= ["name" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_merge"
      , toolDescription = Just "Merge a branch into the current branch"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "branch" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Branch to merge"
                  ]
              ]
          , "required" .= ["branch" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_lib_install"
      , toolDescription = Just "Install a library"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "library" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Library to install (e.g., @unison/base)"
                  ]
              ]
          , "required" .= ["library" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_share_search"
      , toolDescription = Just "Search for libraries on Unison Share"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "query" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Search query for Unison Share"
                  ]
              ]
          , "required" .= ["query" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_share_install"
      , toolDescription = Just "Install a library from Unison Share with full path"
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "library" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Full library path (e.g., @unison/base/releases/3.21.0)"
                  ]
              , "as" .= object
                  [ "type" .= String "string"
                  , "description" .= String "Optional local name for the library"
                  ]
              ]
          , "required" .= ["library" :: Text]
          ]
      }
  , Tool
      { toolName = "ucm_command"
      , toolDescription = Just "Execute any UCM command directly. This is a low-level tool - verify command syntax and type requirements before use. Errors from UCM will be returned as-is."
      , toolInputSchema = object
          [ "type" .= String "object"
          , "properties" .= object
              [ "command" .= object
                  [ "type" .= String "string"
                  , "description" .= String "UCM command (e.g., 'pull', 'push', 'fork', 'reflog', 'help')"
                  ]
              , "args" .= object
                  [ "type" .= String "array"
                  , "items" .= object ["type" .= String "string"]
                  , "description" .= String "Command arguments as separate strings (e.g., ['@unison/base/main', 'my-base'])"
                  ]
              ]
          , "required" .= ["command" :: Text]
          ]
      }
  ]

-- | Handle a tool call
handleToolCall :: MonadIO m => UCMHandle -> ToolCall -> m ToolResult
handleToolCall ucm toolCall = case toolCallName toolCall of
  "ucm_find" -> handleFind ucm (toolCallArguments toolCall)
  "ucm_add" -> handleAdd ucm (toolCallArguments toolCall)
  "ucm_run" -> handleRun ucm (toolCallArguments toolCall)
  "ucm_list_projects" -> handleListProjects ucm
  "ucm_switch_project" -> handleSwitchProject ucm (toolCallArguments toolCall)
  "ucm_list_branches" -> handleListBranches ucm
  "ucm_switch_branch" -> handleSwitchBranch ucm (toolCallArguments toolCall)
  "ucm_dependencies" -> handleDependencies ucm (toolCallArguments toolCall)
  "ucm_view" -> handleView ucm (toolCallArguments toolCall)
  "ucm_update" -> handleUpdate ucm
  "ucm_ls" -> handleLs ucm (toolCallArguments toolCall)
  "ucm_delete" -> handleDelete ucm (toolCallArguments toolCall)
  "ucm_test" -> handleTest ucm (toolCallArguments toolCall)
  "ucm_project_create" -> handleProjectCreate ucm (toolCallArguments toolCall)
  "ucm_branch_create" -> handleBranchCreate ucm (toolCallArguments toolCall)
  "ucm_merge" -> handleMerge ucm (toolCallArguments toolCall)
  "ucm_lib_install" -> handleLibInstall ucm (toolCallArguments toolCall)
  "ucm_share_search" -> handleShareSearch ucm (toolCallArguments toolCall)
  "ucm_share_install" -> handleShareInstall ucm (toolCallArguments toolCall)
  "ucm_command" -> handleCommand ucm (toolCallArguments toolCall)
  _ -> pure $ ToolResult
    { toolResultContent = [textContent $ "Unknown tool: " <> toolCallName toolCall]
    , toolResultIsError = Just True
    }

-- | Implementation of ucm_find
handleFind :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleFind ucm (Just params) = do
  case params of
    Object o -> case lookup "query" (toList o) of
      Just (String query) -> do
        results <- liftIO $ UCM.findDefinitions ucm query
        pure $ ToolResult
          { toolResultContent = [textContent results]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid query parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleFind _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_add
handleAdd :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleAdd ucm (Just params) = do
  case params of
    Object o -> case lookup "code" (toList o) of
      Just (String code) -> do
        result <- liftIO $ UCM.addCode ucm code
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid code parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleAdd _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_run
handleRun :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleRun ucm (Just params) = do
  case params of
    Object o -> case lookup "expression" (toList o) of
      Just (String expr) -> do
        result <- liftIO $ UCM.runExpression ucm expr
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid expression parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleRun _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_list_projects
handleListProjects :: MonadIO m => UCMHandle -> m ToolResult
handleListProjects ucm = do
  projects <- liftIO $ UCM.listProjects ucm
  pure $ ToolResult
    { toolResultContent = [textContent (T.unlines projects)]
    , toolResultIsError = Just False
    }

-- | Implementation of ucm_switch_project
handleSwitchProject :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleSwitchProject ucm (Just params) = do
  case params of
    Object o -> case lookup "project" (toList o) of
      Just (String project) -> do
        result <- liftIO $ UCM.switchProject ucm project
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid project parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleSwitchProject _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_list_branches
handleListBranches :: MonadIO m => UCMHandle -> m ToolResult
handleListBranches ucm = do
  branches <- liftIO $ UCM.listBranches ucm
  pure $ ToolResult
    { toolResultContent = [textContent (T.unlines branches)]
    , toolResultIsError = Just False
    }

-- | Implementation of ucm_switch_branch
handleSwitchBranch :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleSwitchBranch ucm (Just params) = do
  case params of
    Object o -> case lookup "branch" (toList o) of
      Just (String branch) -> do
        result <- liftIO $ UCM.switchBranch ucm branch
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid branch parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleSwitchBranch _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_dependencies
handleDependencies :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleDependencies ucm (Just params) = do
  case params of
    Object o -> case lookup "name" (toList o) of
      Just (String name) -> do
        deps <- liftIO $ UCM.getDependencies ucm name
        pure $ ToolResult
          { toolResultContent = [textContent (T.unlines deps)]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid name parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleDependencies _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_view
handleView :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleView ucm (Just params) = do
  case params of
    Object o -> case lookup "name" (toList o) of
      Just (String name) -> do
        source <- liftIO $ UCM.viewSource ucm name
        pure $ ToolResult
          { toolResultContent = [textContent source]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid name parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleView _ Nothing = pure $ errorResult "Missing parameters"

-- | Helper to create error results
errorResult :: Text -> ToolResult
errorResult msg = ToolResult
  { toolResultContent = [object ["type" .= String "text", "text" .= msg]]
  , toolResultIsError = Just True
  }

-- | Helper to create text content block
textContent :: Text -> Value
textContent text = object ["type" .= String "text", "text" .= text]

-- | Implementation of ucm_update
handleUpdate :: MonadIO m => UCMHandle -> m ToolResult
handleUpdate ucm = do
  result <- liftIO $ UCM.updateDefinitions ucm
  pure $ ToolResult
    { toolResultContent = [textContent result]
    , toolResultIsError = Just False
    }

-- | Implementation of ucm_ls
handleLs :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleLs ucm (Just params) = do
  case params of
    Object o -> case lookup "namespace" (toList o) of
      Just (String ns) -> do
        result <- liftIO $ UCM.listNamespace ucm (Just ns)
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> do
        result <- liftIO $ UCM.listNamespace ucm Nothing
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
    _ -> do
      result <- liftIO $ UCM.listNamespace ucm Nothing
      pure $ ToolResult
        { toolResultContent = [textContent result]
        , toolResultIsError = Just False
        }
handleLs ucm Nothing = do
  result <- liftIO $ UCM.listNamespace ucm Nothing
  pure $ ToolResult
    { toolResultContent = [textContent result]
    , toolResultIsError = Just False
    }

-- | Implementation of ucm_delete
handleDelete :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleDelete ucm (Just params) = do
  case params of
    Object o -> case lookup "name" (toList o) of
      Just (String name) -> do
        result <- liftIO $ UCM.deleteDefinition ucm name
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid name parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleDelete _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_test
handleTest :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleTest ucm (Just params) = do
  case params of
    Object o -> case lookup "pattern" (toList o) of
      Just (String pattern) -> do
        result <- liftIO $ UCM.runTests ucm (Just pattern)
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> do
        result <- liftIO $ UCM.runTests ucm Nothing
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
    _ -> do
      result <- liftIO $ UCM.runTests ucm Nothing
      pure $ ToolResult
        { toolResultContent = [textContent result]
        , toolResultIsError = Just False
        }
handleTest ucm Nothing = do
  result <- liftIO $ UCM.runTests ucm Nothing
  pure $ ToolResult
    { toolResultContent = [textContent result]
    , toolResultIsError = Just False
    }

-- | Implementation of ucm_project_create
handleProjectCreate :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleProjectCreate ucm (Just params) = do
  case params of
    Object o -> case lookup "name" (toList o) of
      Just (String name) -> do
        result <- liftIO $ UCM.createProject ucm name
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid name parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleProjectCreate _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_branch_create
handleBranchCreate :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleBranchCreate ucm (Just params) = do
  case params of
    Object o -> case lookup "name" (toList o) of
      Just (String name) -> do
        result <- liftIO $ UCM.createBranch ucm name
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid name parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleBranchCreate _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_merge
handleMerge :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleMerge ucm (Just params) = do
  case params of
    Object o -> case lookup "branch" (toList o) of
      Just (String branch) -> do
        result <- liftIO $ UCM.mergeBranch ucm branch
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid branch parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleMerge _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_lib_install
handleLibInstall :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleLibInstall ucm (Just params) = do
  case params of
    Object o -> case lookup "library" (toList o) of
      Just (String library) -> do
        result <- liftIO $ UCM.installLibrary ucm library
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid library parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleLibInstall _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_share_search
handleShareSearch :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleShareSearch ucm (Just params) = do
  case params of
    Object o -> case lookup "query" (toList o) of
      Just (String query) -> do
        result <- liftIO $ UCM.searchShare ucm query
        pure $ ToolResult
          { toolResultContent = [textContent result]
          , toolResultIsError = Just False
          }
      _ -> pure $ errorResult "Invalid query parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleShareSearch _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_share_install
handleShareInstall :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleShareInstall ucm (Just params) = do
  case params of
    Object o -> do
      let libraryParam = lookup "library" (toList o)
          asParam = lookup "as" (toList o)
      case libraryParam of
        Just (String library) -> do
          let asName = case asParam of
                Just (String name) -> Just name
                _ -> Nothing
          result <- liftIO $ UCM.installFromShare ucm library asName
          pure $ ToolResult
            { toolResultContent = [textContent result]
            , toolResultIsError = Just False
            }
        _ -> pure $ errorResult "Invalid library parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleShareInstall _ Nothing = pure $ errorResult "Missing parameters"

-- | Implementation of ucm_command
handleCommand :: MonadIO m => UCMHandle -> Maybe Value -> m ToolResult
handleCommand ucm (Just params) = do
  case params of
    Object o -> do
      let cmdParam = lookup "command" (toList o)
          argsParam = lookup "args" (toList o)
      case cmdParam of
        Just (String cmd) -> do
          -- Extract arguments if provided
          let args = case argsParam of
                Just (Array arr) -> 
                  [ arg | String arg <- V.toList arr ]
                _ -> []
          -- Construct full command with arguments
          let fullCommand = if null args
                then cmd
                else cmd <> " " <> T.unwords args
          result <- liftIO $ UCM.sendCommand ucm fullCommand
          pure $ ToolResult
            { toolResultContent = [textContent result]
            , toolResultIsError = Just False
            }
        _ -> pure $ errorResult "Invalid command parameter"
    _ -> pure $ errorResult "Invalid parameters"
handleCommand _ Nothing = pure $ errorResult "Missing parameters"

