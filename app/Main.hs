{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Unison.MCP.Server

main :: IO ()
main = do
  args <- getArgs
  
  -- Get codebase path from args or environment
  codebasePath <- case args of
    [path] -> return path
    [] -> do
      maybeEnv <- lookupEnv "UNISON_CODEBASE"
      case maybeEnv of
        Just path -> return path
        Nothing -> do
          hPutStrLn stderr "Usage: unison-mcp-server <codebase-path>"
          hPutStrLn stderr "Or set UNISON_CODEBASE environment variable"
          exitFailure
    _ -> do
      hPutStrLn stderr "Usage: unison-mcp-server <codebase-path>"
      exitFailure
  
  -- Create server configuration
  let config = MCPConfig
        { configCodebasePath = codebasePath
        , configServerName = "unison-mcp-server"
        , configServerVersion = "0.1.0"
        }
  
  -- Run the server
  runMCPServer config