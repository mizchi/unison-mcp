{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Unison.MCP.UCM
  ( UCMHandle
  , withUCM
  , startUCM
  , stopUCM
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
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, catch, IOException)
import Control.Monad (forever, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO
import System.Process
import System.Exit (ExitCode(..))
import System.Directory (findExecutable, removeFile)

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
  
  (Just stdin, Just stdout, Just stderr, ph) <- 
    createProcess (proc cmd args)
      { std_in = CreatePipe
      , std_out = CreatePipe
      , std_err = CreatePipe
      }
  
  -- Set handles to line buffering mode for better interaction
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  
  -- Create output buffer
  outputVar <- newTVarIO []
  errorVar <- newTVarIO []
  
  let handle = UCMHandle
        { ucmProcess = ph
        , ucmStdin = stdin
        , ucmStdout = stdout
        , ucmStderr = stderr
        , ucmOutput = outputVar
        , ucmErrors = errorVar
        }
  
  -- Start output reader thread with error handling
  _ <- forkIO $ readOutput stdout outputVar
  
  -- Start stderr reader thread (but don't output to stderr during MCP)
  _ <- forkIO $ readError stderr errorVar
  
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
  line <- TIO.hGetLine h `catch` \(e :: IOException) -> do
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
  let waitForOutput attempts = do
        threadDelay 100000  -- 0.1 seconds
        output <- atomically $ readTVar (ucmOutput handle)
        if null output && attempts > 0
          then waitForOutput (attempts - 1)
          else return ()
  
  waitForOutput 10  -- Wait up to 1 second total
  
  -- Collect output
  output <- atomically $ do
    lines <- readTVar (ucmOutput handle)
    writeTVar (ucmOutput handle) []
    return (reverse lines)
  
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
runTests handle Nothing = sendCommand handle "test"
runTests handle (Just pattern) = sendCommand handle $ "test " <> pattern

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

