{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.MCP.UCM
  ( UCMHandle
  , withUCM
  , startUCM
  , stopUCM
  , sendCommand
  , findDefinitions
  , addCode
  , runExpression
  , listProjects
  , switchProject
  , listBranches
  , switchBranch
  , getDependencies
  , viewSource
  , updateDefinitions
  , listNamespace
  , deleteDefinition
  , runTests
  , createProject
  , createBranch
  , mergeBranch
  , installLibrary
  , searchShare
  , installFromShare
  , viewScratchFile
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forever, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Process
import System.Directory (findExecutable, removeFile, doesFileExist)

-- | Handle to a running UCM process
data UCMHandle = UCMHandle
  { ucmProcess :: ProcessHandle
  , ucmStdin :: Handle
  , ucmStdout :: Handle
  , ucmStderr :: Handle
  , ucmOutput :: TVar [Text]
  , ucmErrors :: TVar [Text]
  }

-- | Start UCM process
startUCM :: FilePath -> IO UCMHandle
startUCM codebasePath = do
  -- Check if UCM is available
  ucmPath <- findExecutable "ucm"
  case ucmPath of
    Nothing -> error "UCM executable not found in PATH"
    Just _ -> return ()
  
  -- Remove lock file if it exists
  let lockFile = codebasePath ++ "/.unison/v2/unison.lockfile"
  removeFile lockFile `catch` \(_ :: IOException) -> return ()
  
  let cmd = "ucm"
      args = ["--codebase-create", codebasePath, "--no-file-watch"]
  
  (Just ucmStdin, Just ucmStdout, Just ucmStderr, ph) <- 
    createProcess (proc cmd args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  
  -- Set handles to line buffering mode for better interaction
  hSetBuffering ucmStdin LineBuffering
  hSetBuffering ucmStdout LineBuffering
  hSetBuffering ucmStderr LineBuffering
  
  -- Create output buffer
  outputVar <- newTVarIO []
  errorVar <- newTVarIO []
  
  let handle = UCMHandle
        { ucmProcess = ph
        , ucmStdin = ucmStdin
        , ucmStdout = ucmStdout
        , ucmStderr = ucmStderr
        , ucmOutput = outputVar
        , ucmErrors = errorVar
        }
  
  -- Start output reader thread with error handling
  _ <- forkIO $ readOutput ucmStdout outputVar
  
  -- Start stderr reader thread (but don't output to stderr during MCP)
  _ <- forkIO $ readError ucmStderr errorVar
  
  -- Check if process started successfully
  exitCode <- getProcessExitCode ph
  case exitCode of
    Just code -> error $ "UCM process exited immediately with code: " ++ show code
    Nothing -> do
      -- Wait for UCM to be ready
      waitForPrompt handle
      return handle

-- | Stop UCM process
stopUCM :: UCMHandle -> IO ()
stopUCM handle = do
  -- Try to close handles gracefully
  hClose (ucmStdin handle) `catch` \(_ :: IOException) -> return ()
  hClose (ucmStdout handle) `catch` \(_ :: IOException) -> return ()
  hClose (ucmStderr handle) `catch` \(_ :: IOException) -> return ()
  terminateProcess (ucmProcess handle)

-- | Start UCM and provide a handle for interaction
withUCM :: FilePath -> (UCMHandle -> IO a) -> IO a
withUCM codebasePath action = do
  bracket (startUCM codebasePath) stopUCM action

-- | Read output with error handling
readOutput :: Handle -> TVar [Text] -> IO ()
readOutput h outputVar = forever $ do
  line <- TIO.hGetLine h `catch` \(e :: IOException) -> do
    hPutStrLn stderr $ "Error reading stdout: " ++ show e
    return ""
  when (not $ T.null line) $
    atomically $ modifyTVar outputVar (line:)

-- | Read stderr output (silently discard during MCP operation)
readError :: Handle -> TVar [Text] -> IO ()
readError h errorVar = forever $ do
  line <- TIO.hGetLine h `catch` \(_ :: IOException) -> do
    -- Silently ignore errors during MCP operation
    return ""
  -- Store stderr output for debugging
  when (not $ T.null line) $
    atomically $ modifyTVar errorVar (line:)

-- | Wait for UCM prompt
waitForPrompt :: UCMHandle -> IO ()
waitForPrompt handle = do
  -- Give UCM time to start up
  threadDelay 3000000  -- 3 seconds
  
  -- Check if process is still running
  exitCode <- getProcessExitCode (ucmProcess handle)
  case exitCode of
    Just code -> do
      -- Get stderr output for debugging
      errors <- atomically $ readTVar (ucmErrors handle)
      let errorMsg = T.unlines (reverse errors)
      error $ "UCM process exited during initialization with code: " ++ show code ++ "\nStderr: " ++ T.unpack errorMsg
    Nothing -> do
      -- Clear any initial output
      atomically $ writeTVar (ucmOutput handle) []
      return ()

-- | Send a command to UCM and get the response
sendCommand :: UCMHandle -> Text -> IO Text
sendCommand handle cmd = do
  -- Clear output buffer
  atomically $ writeTVar (ucmOutput handle) []
  
  -- Send command
  TIO.hPutStrLn (ucmStdin handle) cmd
  hFlush (ucmStdin handle)
  
  -- Wait for response with multiple attempts
  let waitForOutput :: Int -> IO ()
      waitForOutput attempts = do
        threadDelay 100000  -- 0.1 seconds
        output <- atomically $ readTVar (ucmOutput handle)
        if null output && attempts > 0
          then waitForOutput (attempts - 1)
          else return ()
  
  waitForOutput 10  -- Wait up to 1 second total
  
  -- Collect output
  output <- atomically $ do
    outputLines <- readTVar (ucmOutput handle)
    writeTVar (ucmOutput handle) []
    return (reverse outputLines)
  
  return $ T.unlines output

-- | Find definitions in the codebase
findDefinitions :: UCMHandle -> Text -> IO Text
findDefinitions handle query = 
  sendCommand handle $ "find " <> query

-- | Add code to the codebase
addCode :: UCMHandle -> Text -> IO Text
addCode handle code = do
  -- Write code to a temporary file in current directory
  let scratchFile = "temp_scratch.u"
  TIO.writeFile scratchFile code
  
  -- Load the file
  loadOutput <- sendCommand handle $ "load " <> T.pack scratchFile
  
  -- Check if load was successful
  if T.isInfixOf "loaded successfully" loadOutput || T.isInfixOf "typechecked" loadOutput
    then do
      -- Add the definitions
      addOutput <- sendCommand handle "add"
      -- Clean up
      removeFile scratchFile `catch` \(_ :: IOException) -> return ()
      return $ loadOutput <> "\n" <> addOutput
    else do
      -- Clean up even on failure
      removeFile scratchFile `catch` \(_ :: IOException) -> return ()
      return loadOutput

-- | Run a Unison expression
runExpression :: UCMHandle -> Text -> IO Text
runExpression handle expr =
  sendCommand handle $ "run " <> expr

-- | List all projects
listProjects :: UCMHandle -> IO [Text]
listProjects handle = do
  output <- sendCommand handle "projects"
  -- Parse the output to extract project names
  return $ T.lines output

-- | Switch to a project
switchProject :: UCMHandle -> Text -> IO Text
switchProject handle project =
  sendCommand handle $ "project.switch " <> project

-- | List branches in current project
listBranches :: UCMHandle -> IO [Text]
listBranches handle = do
  output <- sendCommand handle "branches"
  -- Parse the output to extract branch names
  return $ T.lines output

-- | Switch to a branch
switchBranch :: UCMHandle -> Text -> IO Text
switchBranch handle branch =
  sendCommand handle $ "switch " <> branch

-- | Get dependencies of a definition
getDependencies :: UCMHandle -> Text -> IO [Text]
getDependencies handle name = do
  output <- sendCommand handle $ "dependencies " <> name
  return $ T.lines output

-- | View source of a definition
viewSource :: UCMHandle -> Text -> IO Text
viewSource handle name =
  sendCommand handle $ "view " <> name

-- | Update definitions in the codebase
updateDefinitions :: UCMHandle -> IO Text
updateDefinitions handle =
  sendCommand handle "update"

-- | List contents of a namespace
listNamespace :: UCMHandle -> Maybe Text -> IO Text
listNamespace handle Nothing = sendCommand handle "ls"
listNamespace handle (Just ns) = sendCommand handle $ "ls " <> ns

-- | Delete a definition
deleteDefinition :: UCMHandle -> Text -> IO Text
deleteDefinition handle name =
  sendCommand handle $ "delete " <> name

-- | Run tests
runTests :: UCMHandle -> Maybe Text -> IO Text
runTests handle Nothing = do
  -- Check if scratch.u exists
  scratchExists <- doesFileExist "scratch.u"
  if scratchExists
    then do
      -- Load scratch.u and run tests
      loadResult <- sendCommand handle "load scratch.u"
      if T.isInfixOf "typechecked" loadResult || T.isInfixOf "loaded" loadResult
        then sendCommand handle "test"
        else return loadResult
    else do
      -- Create a default scratch.u with sample tests
      TIO.writeFile "scratch.u" defaultScratchContent
      loadResult <- sendCommand handle "load scratch.u"
      if T.isInfixOf "typechecked" loadResult || T.isInfixOf "loaded" loadResult
        then do
          testResult <- sendCommand handle "test"
          return $ "Created scratch.u with sample tests\n" <> loadResult <> "\n" <> testResult
        else return loadResult
runTests handle (Just pattern) = sendCommand handle $ "test " <> pattern

-- | Default content for scratch.u
defaultScratchContent :: Text
defaultScratchContent = T.unlines
  [ "-- Unison scratch file for testing"
  , "-- This file was auto-generated by ucm_test"
  , "-- Edit this file to add your own tests"
  , ""
  , "-- Import test utilities"
  , "use lib.base.test.check"
  , ""
  , "-- Sample function"
  , "add : Nat -> Nat -> Nat"
  , "add x y = x + y"
  , ""
  , "-- Sample property test"
  , "test.add.commutative : [Result]" 
  , "test.add.commutative ="
  , "  [ check (add 2 3 == add 3 2)"
  , "  , check (add 0 5 == add 5 0)"
  , "  , check (add 10 20 == add 20 10)"
  , "  ]"
  , ""
  , "test.add.identity : [Result]"
  , "test.add.identity ="
  , "  [ check (add 0 0 == 0)"
  , "  , check (add 5 0 == 5)"
  , "  , check (add 0 10 == 10)"
  , "  ]"
  , ""
  , "-- To run tests: test"
  , "-- To run specific test: test test.add.commutative"
  ]

-- | Create a new project
createProject :: UCMHandle -> Text -> IO Text
createProject handle projectName =
  sendCommand handle $ "project.create " <> projectName

-- | Create a new branch
createBranch :: UCMHandle -> Text -> IO Text
createBranch handle branchName =
  sendCommand handle $ "branch.create " <> branchName

-- | Merge a branch
mergeBranch :: UCMHandle -> Text -> IO Text
mergeBranch handle branchName =
  sendCommand handle $ "merge " <> branchName

-- | Install a library
installLibrary :: UCMHandle -> Text -> IO Text
installLibrary handle libraryName =
  sendCommand handle $ "lib.install " <> libraryName

-- | Search for libraries on Unison Share
searchShare :: UCMHandle -> Text -> IO Text
searchShare handle query = do
  -- Use the `find` command with special syntax for Share search
  result <- sendCommand handle $ "find.global " <> query
  -- If find.global doesn't work, try alternative approach
  if T.isInfixOf "unknown command" result || T.isInfixOf "I don't know" result
    then do
      -- Try using lib.search if available
      searchResult <- sendCommand handle $ "lib.search " <> query
      if T.isInfixOf "unknown command" searchResult || T.isInfixOf "I don't know" searchResult
        then return $ "Note: UCM does not support direct Share search. Try:\n" <>
                     "1. Browse https://share.unison-lang.org/ to find libraries\n" <>
                     "2. Use 'lib.install @owner/library' if you know the library path\n" <>
                     "3. After install, check 'ls lib' to see installed libraries\n" <>
                     "4. Installed libraries follow naming convention: @owner/library -> lib.owner_library_version\n" <>
                     "5. Common libraries:\n" <>
                     "   - @unison/base - Base library with core types and functions\n" <>
                     "   - @unison/http - HTTP client/server functionality\n" <>
                     "   - @unison/distributed - Distributed computing utilities\n" <>
                     "   - @unison/cli - Command-line interface utilities\n" <>
                     "   - @hojberg/nanoid - NanoID unique string generator"
        else return searchResult
    else return result

-- | Install a library from Share with optional local name
installFromShare :: UCMHandle -> Text -> Maybe Text -> IO Text
installFromShare handle libraryPath asName = do
  let command = case asName of
        Nothing -> "lib.install " <> libraryPath
        Just name -> "lib.install " <> libraryPath <> " " <> name
  sendCommand handle command

-- | View the contents of scratch.u file
viewScratchFile :: IO Text
viewScratchFile = do
  scratchExists <- doesFileExist "scratch.u"
  if scratchExists
    then TIO.readFile "scratch.u"
    else return "scratch.u file not found. Create one with 'ucm_test' or manually."

