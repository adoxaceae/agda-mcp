{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module AgdaMCP.Server
  ( ServerState(..)
  , initServerState
  , handleAgdaTool
  , handleAgdaResource
  , initSessionManager
  , handleAgdaToolWithSession
  , handleAgdaResourceWithSession
  , findProjectRoot
  ) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON.Key
import qualified AgdaMCP.Format as Format
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.KeyMap as JSON.KeyMap
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.Text (Text)
import Data.IORef
import GHC.Generics (Generic)
import Control.Monad (forM, void, unless)
import qualified Data.List
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (catchError)
import qualified Control.Exception
import System.IO (hPutStrLn, stderr)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (race, async, waitCatch, Async)
import System.Timeout (timeout)
import System.FilePath (takeDirectory)
import System.Directory (listDirectory)

-- MCP Server library
import qualified MCP.Server
import qualified AgdaMCP.Types
import qualified AgdaMCP.Repl as Repl
import qualified AgdaMCP.SessionManager as SessionManager
import qualified AgdaMCP.FileEdit as FileEdit

-- Agda imports - using the actual interaction functions
import Agda.Interaction.Base
import Agda.Interaction.BasicOps as BasicOps
import Agda.Interaction.MakeCase (makeCase)
import Agda.Interaction.Imports (CheckResult, crInterface, Mode(..), parseSource, typeCheckMain)
import Agda.Interaction.Options (defaultOptions, CommandLineOptions(..))
import Agda.Interaction.Output (OutputConstraint)
import Agda.Interaction.Response
import Agda.Interaction.JSON (EncodeTCM(..), encodeTCM)
import Agda.Interaction.JSONTop () -- Import JSON instances

import Agda.Syntax.Common
import Agda.Syntax.Common.Pretty (render, prettyShow)
import Agda.Syntax.Position (Range, rStart, rEnd, posLine, posCol, noRange)
import Agda.Syntax.Scope.Base (WhyInScopeData(..))
import Agda.Syntax.Abstract (Expr)
import Agda.Syntax.Abstract.Name (QName(..), nameBindingSite, qnameName)
-- import Agda.Syntax.Translation.AbstractToConcrete (abstractToConcrete_) -- redundant

import Agda.TypeChecking.Monad
-- import Agda.TypeChecking.Monad.Options (getAgdaLibFilesWithoutTopLevelModuleName, setLibraryIncludes) -- redundant
-- import Agda.TypeChecking.Monad.MetaVars (getInteractionRange) -- redundant
import Agda.TypeChecking.Pretty (prettyTCM, prettyA)

import Agda.Utils.FileName (absolute, filePath)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (catMaybes)

-- Data structures for MCP communication
data AgdaGoal = AgdaGoal
  { goalId :: Int
  , goalType :: Text
  , goalRange :: (Int, Int, Int, Int)
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaGoal
instance JSON.FromJSON AgdaGoal

data AgdaContextEntry = AgdaContextEntry
  { entryName :: Text
  , entryType :: Text
  , entryValue :: Maybe Text
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaContextEntry
instance JSON.FromJSON AgdaContextEntry

data PostulateInfo = PostulateInfo
  { pName :: Text
  , pType :: Text
  , pRange :: (Int, Int, Int, Int)
  } deriving (Generic, Show)

instance JSON.ToJSON PostulateInfo
instance JSON.FromJSON PostulateInfo

data AgdaResult = AgdaResult
  { success :: Bool
  , message :: Text
  , agdaResult :: Maybe JSON.Value
  } deriving (Generic, Show)

instance JSON.ToJSON AgdaResult
instance JSON.FromJSON AgdaResult

-- | Wrapper for commands with response handling
data CommandWithResponse = CommandWithResponse
  { cmdIOTCM :: IOTCM
  , cmdResponseVar :: MVar JSON.Value  -- Store encoded JSON, not raw Response
  , cmdHandler :: Maybe ResponseHandler -- Optional custom handler for responses
  }

-- Server state - tracks current file, check result, highlighting info, and persistent REPL
data ServerState = ServerState
  { currentFile :: Maybe AbsolutePath
  , currentLibraryFile :: Maybe FilePath     -- Persisted library file path
  , checkResult :: Maybe CheckResult
  , highlightingInfo :: Maybe JSON.Value     -- Cached highlighting info for goto-def/find-refs
  , commandChan :: Chan CommandWithResponse  -- Channel to send commands to REPL
  , replAsync :: Maybe (Async ())            -- Background REPL thread handle
  , shutdownVar :: MVar ()                   -- Shutdown signal for graceful termination
  , currentResponseVar :: IORef (Maybe (MVar JSON.Value, Maybe ResponseHandler))  -- Current response context
  }

-- | Function that processes a response in the TCM monad and optionally returns a JSON result
type ResponseHandler = Response -> TCM (Maybe JSON.Value)

-- ============================================================================
-- REPL Integration - Persistent Session Management
-- ============================================================================

-- | Read IOTCM commands from channel, store response MVar, and wrap in Command type
-- Uses race to allow graceful shutdown when shutdownVar is signaled
-- | Read IOTCM commands from channel, store response MVar, and wrap in Command type
-- Uses race to allow graceful shutdown when shutdownVar is signaled
readCommandFromChan :: IORef (Maybe (MVar JSON.Value, Maybe ResponseHandler)) -> MVar () -> Chan CommandWithResponse -> IO Command
readCommandFromChan responseVarRef shutdownVar chan = do
  result <- race (takeMVar shutdownVar) (readChan chan)
  case result of
    Left () -> do
      -- Shutdown signal received, exit REPL loop gracefully
      hPutStrLn stderr "REPL received shutdown signal, exiting gracefully"
      return Done
    Right (CommandWithResponse iotcm responseVar handler) -> do
      -- Store the response MVar and optional handler so callback can use it
      writeIORef responseVarRef (Just (responseVar, handler))
      return $ Command iotcm

-- | Callback for capturing Agda responses and routing to waiting handlers
-- Pattern match on specific response types we care about
-- Encode responses in the TCM context before routing
-- If a custom handler is active, it gets first dibs on the response
mcpCallback :: IORef ServerState -> IORef (Maybe (MVar JSON.Value, Maybe ResponseHandler)) -> Response -> TCM ()
mcpCallback stateRef responseVarRef resp = do
  let isContentResponse = case resp of
        Resp_InteractionPoints _ -> True
        Resp_DisplayInfo _ -> True
        Resp_GiveAction _ _ -> True
        Resp_MakeCase _ _ _ -> True
        Resp_SolveAll _ -> True
        Resp_JumpToError _ _ -> True
        Resp_Status _ -> False
        Resp_HighlightingInfo _ _ _ _ -> True  -- Now capturing highlighting info
        Resp_RunningInfo _ _ -> False
        Resp_ClearRunningInfo -> False
        Resp_ClearHighlighting _ -> False
        Resp_DoneAborting -> False
        Resp_DoneExiting -> False
        Resp_Mimer _ _ -> False

  if isContentResponse
    then do
      -- NEW: Extract file edits BEFORE encoding
      -- This works with typed Response values in TCM context
      fileEdits <- extractFileEditsFromResponse stateRef resp

      -- Get the current response context
      maybeContext <- liftIO $ readIORef responseVarRef
      
      -- Check if we have a custom handler
      handled <- case maybeContext of
        Just (responseVar, Just handler) -> do
           -- Try the custom handler
           maybeResult <- handler resp
           case maybeResult of
             Just result -> liftIO $ do
               -- Handler produced a result! Send it.
               success <- tryPutMVar responseVar result
               if success
                 then do
                   hPutStrLn stderr "Custom handler processed response"
                   writeIORef responseVarRef Nothing -- Clear context as we are done
                   return True
                 else do
                   hPutStrLn stderr "Custom handler result ignored (response already sent)"
                   return True
             Nothing -> return False -- Handler didn't handle this response
        _ -> return False

      unless handled $ do
        -- Apply file edits if any
        unless (null fileEdits) $ liftIO $ do
          hPutStrLn stderr $ "Applying " ++ show (length fileEdits) ++ " file edit(s)"
          FileEdit.applyFileEdits fileEdits

        -- Continue with existing flow: encode response
        jsonValue <- encodeTCM resp

        -- Special handling for highlighting info - store in state instead of routing
        case resp of
          Resp_HighlightingInfo _ _ _ _ -> liftIO $ do
            -- Debug: Log first 3000 chars of highlighting JSON
            let jsonText = TE.decodeUtf8 $ LBS.toStrict $ JSON.encode jsonValue
            hPutStrLn stderr $ "Highlighting JSON (first 3000 chars): " ++ take 3000 (T.unpack jsonText)
            modifyIORef stateRef (\s -> s { highlightingInfo = Just jsonValue })
            hPutStrLn stderr "Highlighting info captured and stored in state"
          _ -> do
            -- Route to waiting handler for other content responses
            maybeContext <- liftIO $ readIORef responseVarRef
            case maybeContext of
              Just (responseVar, _) -> liftIO $ do
                success <- tryPutMVar responseVar jsonValue
                if success
                  then do
                    hPutStrLn stderr "Agda response encoded and routed to handler"
                    writeIORef responseVarRef Nothing
                  else
                    hPutStrLn stderr "Agda response ignored (handler already has response)"
              Nothing -> liftIO $
                hPutStrLn stderr "Warning: Received response with no waiting handler"
    else liftIO $
      hPutStrLn stderr "Agda non-content response (skipped)"

-- ============================================================================
-- File Edit Extraction (in TCM context)
-- ============================================================================

-- | Extract file edits from Response in TCM context
-- This runs BEFORE encoding to JSON, using typed Response values
extractFileEditsFromResponse :: IORef ServerState -> Response -> TCM [FileEdit.FileEdit]
extractFileEditsFromResponse stateRef resp = do
  state <- liftIO $ readIORef stateRef
  let maybeFile = currentFile state

  case (resp, maybeFile) of
    -- Give: In-place hole replacement
    (Resp_GiveAction ii (Give_String str), Just file) -> do
      range <- getInteractionRange ii
      let filepath = filePath file
      liftIO $ hPutStrLn stderr $ "Extracting GiveAction (Give_String) for goal " ++ show ii
      return [FileEdit.ReplaceHole filepath range (T.pack str) False]

    (Resp_GiveAction ii Give_Paren, Just file) -> do
      range <- getInteractionRange ii
      let filepath = filePath file
      liftIO $ hPutStrLn stderr $ "Extracting GiveAction (Give_Paren) for goal " ++ show ii
      return [FileEdit.ReplaceHole filepath range "" True]

    (Resp_GiveAction ii Give_NoParen, Just file) -> do
      -- Give_NoParen: don't remove braces, just leave content as-is
      -- This means we don't need to edit the file at all
      liftIO $ hPutStrLn stderr $ "GiveAction (Give_NoParen) for goal " ++ show ii ++ " - no file edit needed"
      return []

    -- MakeCase: Line replacement + multiple clauses
    (Resp_MakeCase ii variant clauses, Just file) -> do
      range <- getInteractionRange ii
      let filepath = filePath file
      let (lineNum, col, _, _) = FileEdit.rangeToPositions range
      -- Get indentation from column position
      let indentLevel = col - 1
      liftIO $ hPutStrLn stderr $ "Extracting MakeCase for goal " ++ show ii ++
                          " at line " ++ show lineNum ++
                          " with " ++ show (length clauses) ++ " clauses"
      return [FileEdit.ReplaceLine filepath lineNum (map T.pack clauses) indentLevel True]

    -- SolveAll: Batch of hole replacements
    (Resp_SolveAll solutions, Just file) -> do
      let filepath = filePath file
      liftIO $ hPutStrLn stderr $ "Extracting SolveAll with " ++ show (length solutions) ++ " solutions"

      -- Extract edits for each solution
      -- Note: solutions are already concrete Expr (not abstract)
      ops <- forM solutions $ \(ii, expr) -> do
        range <- getInteractionRange ii
        -- expr is already Concrete.Expr, just prettyShow it
        let exprStr = prettyShow expr
        liftIO $ hPutStrLn stderr $ "  Solution for goal " ++ show ii ++ ": " ++ exprStr
        return $ FileEdit.ReplaceHole filepath range (T.pack exprStr) False

      -- Sort in reverse position order (bottom to top)
      let sorted = Data.List.sortBy FileEdit.compareFileEditReverse ops
      return [FileEdit.BatchEdits filepath sorted]

    -- No file edits for other response types
    _ -> do
      liftIO $ hPutStrLn stderr $ "extractFileEditsFromResponse: No file edits for response type: " ++ show (responseType resp)
      return []
  where
    responseType :: Response -> String
    responseType (Resp_HighlightingInfo _ _ _ _) = "HighlightingInfo"
    responseType (Resp_Status _) = "Status"
    responseType (Resp_JumpToError _ _) = "JumpToError"
    responseType (Resp_InteractionPoints _) = "InteractionPoints"
    responseType (Resp_GiveAction _ _) = "GiveAction"
    responseType (Resp_MakeCase _ _ _) = "MakeCase"
    responseType (Resp_SolveAll _) = "SolveAll"
    responseType (Resp_Mimer _ _) = "Mimer"
    responseType (Resp_DisplayInfo _) = "DisplayInfo"
    responseType (Resp_RunningInfo _ _) = "RunningInfo"
    responseType Resp_ClearRunningInfo = "ClearRunningInfo"
    responseType (Resp_ClearHighlighting _) = "ClearHighlighting"
    responseType Resp_DoneAborting = "DoneAborting"
    responseType Resp_DoneExiting = "DoneExiting"

-- | Convert MCP tool to Agda IOTCM command
-- Takes the current file path (empty string if no file loaded)
-- Note: The format field is ignored here; it's only used for response formatting
toolToIOTCM :: String -> AgdaMCP.Types.AgdaTool -> IOTCM
toolToIOTCM currentFilePath tool =
  -- IOTCM type is: Maybe TopLevelModuleName -> IOTCM' Range
  -- So we need to return a function that ignores the module name parameter
  \_ -> case tool of
    AgdaMCP.Types.AgdaLoad{file, libraryFile} ->
      -- Pass --library-file option if provided
      let opts = case libraryFile of
                   Just lf -> ["--library-file=" ++ T.unpack lf]
                   Nothing -> []
      in IOTCM (T.unpack file) None Direct (Cmd_load (T.unpack file) opts)
    AgdaMCP.Types.AgdaGoalAtPosition{file} ->
      -- Maps to Cmd_metas to trigger interaction points response, handled by custom handler
      IOTCM (T.unpack file) None Direct (Cmd_metas Simplified)
    AgdaMCP.Types.AgdaListPostulates{file} ->
      -- Maps to Cmd_load to ensure file is loaded, post-processing done by handler
      IOTCM (T.unpack file) None Direct (Cmd_load (T.unpack file) [])
    AgdaMCP.Types.AgdaGotoDefinition{file} ->
      -- Maps to Cmd_load to ensure file loaded + highlighting, handler does the rest
      IOTCM (T.unpack file) None Direct (Cmd_load (T.unpack file) [])
    AgdaMCP.Types.AgdaGetGoals{} ->
      -- Goals operate on currently loaded file (format field ignored)
      IOTCM currentFilePath None Direct (Cmd_metas Simplified)
    AgdaMCP.Types.AgdaGetGoalType{goalId} ->
      -- Goal operations use current file, noRange is acceptable since REPL tracks interaction points
      IOTCM currentFilePath None Direct (Cmd_goal_type Simplified (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaGetGoalTypeImplicits{goalId} ->
      -- Show goal type with implicit arguments using Instantiated rewrite mode
      IOTCM currentFilePath None Direct (Cmd_goal_type Instantiated (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaGetContext{goalId} ->
      IOTCM currentFilePath None Direct (Cmd_context Simplified (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaGetContextImplicits{goalId} ->
      -- Show context with implicit arguments using Instantiated rewrite mode
      IOTCM currentFilePath None Direct (Cmd_context Instantiated (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaGive{goalId, expression} ->
      IOTCM currentFilePath None Direct (Cmd_give WithoutForce (InteractionId goalId) noRange (T.unpack expression))
    AgdaMCP.Types.AgdaRefine{goalId, expression} ->
      IOTCM currentFilePath None Direct (Cmd_refine (InteractionId goalId) noRange (T.unpack expression))
    AgdaMCP.Types.AgdaCaseSplit{goalId, variable} ->
      IOTCM currentFilePath None Direct (Cmd_make_case (InteractionId goalId) noRange (T.unpack variable))
    AgdaMCP.Types.AgdaCompute{expression} ->
      IOTCM currentFilePath None Direct (Cmd_compute_toplevel DefaultCompute (T.unpack expression))
    AgdaMCP.Types.AgdaInferType{expression} ->
      IOTCM currentFilePath None Direct (Cmd_infer_toplevel Simplified (T.unpack expression))
    AgdaMCP.Types.AgdaIntro{goalId} ->
      IOTCM currentFilePath None Direct (Cmd_intro False (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaAuto{goalId} ->
      IOTCM currentFilePath None Direct (Cmd_autoOne Simplified (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaAutoAll{} ->
      IOTCM currentFilePath None Direct (Cmd_autoAll Simplified)
    AgdaMCP.Types.AgdaSolveOne{goalId} ->
      IOTCM currentFilePath None Direct (Cmd_solveOne Simplified (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaHelperFunction{goalId, helperName} ->
      IOTCM currentFilePath None Direct (Cmd_helper_function Simplified (InteractionId goalId) noRange (T.unpack helperName))
    AgdaMCP.Types.AgdaGoalTypeContext{goalId} ->
      IOTCM currentFilePath None Direct (Cmd_goal_type_context Simplified (InteractionId goalId) noRange "")
    AgdaMCP.Types.AgdaSearchAbout{query} ->
      IOTCM currentFilePath None Direct (Cmd_search_about_toplevel Simplified (T.unpack query))
    AgdaMCP.Types.AgdaShowModule{moduleName} ->
      IOTCM currentFilePath None Direct (Cmd_show_module_contents_toplevel Simplified (T.unpack moduleName))
    AgdaMCP.Types.AgdaShowConstraints{} ->
      IOTCM currentFilePath None Direct Cmd_constraints
    AgdaMCP.Types.AgdaWhyInScope{name} ->
      IOTCM currentFilePath None Direct (Cmd_why_in_scope_toplevel (T.unpack name))

-- ============================================================================
-- STDIN-based Server (for testing/development)
-- ============================================================================

-- | Alternative STDIN-based server entry point
-- Useful for testing Agda commands without HTTP overhead
runAgdaMCPServer :: IO ()
runAgdaMCPServer = do
  hPutStrLn stderr "Agda MCP Server started. Send JSON commands to stdin."
  stateRef <- initServerState  -- Use same initialization as HTTP server
  serverLoop stateRef

-- Server loop
serverLoop :: IORef ServerState -> IO ()
serverLoop stateRef = do
  line <- getLine
  case JSON.decode (LBS.fromStrict $ TE.encodeUtf8 $ T.pack line) of
    Nothing -> do
      let errorResult = AgdaResult False "Invalid JSON command" Nothing
      hPutStrLn stderr (T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode errorResult)
      serverLoop stateRef
    Just cmd -> do
      result <- runTCMTop $ processMCPCommand stateRef cmd
      case result of
        Left err -> do
          let errorResult = AgdaResult False (T.pack $ show err) Nothing
          hPutStrLn stderr (T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode errorResult)
        Right res ->
          hPutStrLn stderr (T.unpack $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode res)
      serverLoop stateRef

-- Process MCP commands
processMCPCommand :: IORef ServerState -> JSON.Value -> TCM AgdaResult
processMCPCommand stateRef = \case
  JSON.Object obj -> case KM.lookup "command" obj of
    Just (JSON.String "load") ->
      case (KM.lookup "file" obj, KM.lookup "library_file" obj) of
        (Just (JSON.String file), Just (JSON.String libFile)) ->
          mcpLoadFile stateRef (T.unpack file) (Just libFile)
        (Just (JSON.String file), _) ->
          mcpLoadFile stateRef (T.unpack file) Nothing
        _ -> pure $ AgdaResult False "Missing file parameter" Nothing

    Just (JSON.String "get_goals") ->
      mcpGetGoals stateRef

    Just (JSON.String "get_goal_type") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpGetGoalType stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "get_context") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpGetContext stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "give") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpGive stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "refine") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpRefine stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "case_split") ->
      case (KM.lookup "goal_id" obj, KM.lookup "variable" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String var)) ->
          mcpCaseSplit stateRef (floor goalNum) (T.unpack var)
        _ -> pure $ AgdaResult False "Missing goal_id or variable parameter" Nothing

    Just (JSON.String "compute") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpCompute stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "infer_type") ->
      case (KM.lookup "goal_id" obj, KM.lookup "expression" obj) of
        (Just (JSON.Number goalNum), Just (JSON.String expr)) ->
          mcpInferType stateRef (floor goalNum) (T.unpack expr)
        _ -> pure $ AgdaResult False "Missing goal_id or expression parameter" Nothing

    Just (JSON.String "intro") ->
      case KM.lookup "goal_id" obj of
        Just (JSON.Number goalNum) -> mcpIntro stateRef (floor goalNum)
        _ -> pure $ AgdaResult False "Missing goal_id parameter" Nothing

    Just (JSON.String "why_in_scope") ->
      case KM.lookup "name" obj of
        Just (JSON.String name) -> mcpWhyInScope stateRef (T.unpack name)
        _ -> pure $ AgdaResult False "Missing name parameter" Nothing

    _ -> pure $ AgdaResult False "Unknown command" Nothing
  _ -> pure $ AgdaResult False "Invalid command format" Nothing

-- Command implementations

-- Helper function to ensure a file is loaded and scope is set
-- Restores scope from stored CheckResult if available
ensureFileLoaded :: IORef ServerState -> TCM ()
ensureFileLoaded stateRef = do
  state <- liftIO $ readIORef stateRef
  case checkResult state of
    Just checked ->
      -- Restore scope from cached CheckResult
      setScope $ iInsideScope $ crInterface checked
    Nothing ->
      -- No file loaded yet
      genericError "No file loaded. Use agda_load first."

-- Helper function to load a file with caching
-- If the file is already loaded in state, reuse the cached CheckResult
-- Otherwise, parse and type-check the file
loadFileWithCache :: IORef ServerState -> FilePath -> Maybe Text -> TCM CheckResult
loadFileWithCache stateRef filepath libraryFileArg = do
  absPath <- liftIO $ absolute filepath
  state <- liftIO $ readIORef stateRef

  -- Determine effective library file: argument overrides state
  let effectiveLibFile = case libraryFileArg of
        Just lf -> Just (T.unpack lf)
        Nothing -> currentLibraryFile state

  -- Check if this file is already loaded
  case (currentFile state, checkResult state) of
    (Just cachedPath, Just cached) | cachedPath == absPath -> do
      liftIO $ hPutStrLn stderr $ "Using cached result for: " ++ filepath
      setScope $ iInsideScope $ crInterface cached
      return cached
    _ -> do
      -- Not cached or different file - load it fresh
      liftIO $ hPutStrLn stderr $ "Loading file fresh: " ++ filepath

      -- Find project root (directory containing .agda-lib file)
      maybeProjectRoot <- liftIO $ findProjectRoot filepath
      workDir <- case maybeProjectRoot of
        Just root -> do
          liftIO $ hPutStrLn stderr $ "Found project root: " ++ root
          liftIO $ absolute root
        Nothing -> do
          liftIO $ hPutStrLn stderr $ "No project root found, using file directory"
          liftIO $ absolute $ takeDirectory filepath

      -- Discover and load .agda-lib files for this file's project
      _ <- getAgdaLibFilesWithoutTopLevelModuleName absPath

      -- Set up library includes (adds library paths to options)
      let defaultOptsWithLibFile = case effectiveLibFile of
            Just lf -> defaultOptions { optOverrideLibrariesFile = Just lf }
            Nothing -> defaultOptions
      optsWithLibs <- setLibraryIncludes defaultOptsWithLibFile
      setCommandLineOptions' workDir optsWithLibs

      fileId <- idFromFile absPath
      let sourceFile = SourceFile fileId
      source <- parseSource sourceFile
      checked <- typeCheckMain TypeCheck source
      setScope $ iInsideScope $ crInterface checked

      -- Update state with new load
      liftIO $ writeIORef stateRef (state { currentFile = Just absPath, checkResult = Just checked })
      return checked

mcpLoadFile :: IORef ServerState -> FilePath -> Maybe Text -> TCM AgdaResult
mcpLoadFile stateRef file libraryFile = do
  absPath <- liftIO $ absolute file

  -- Find project root (directory containing .agda-lib file)
  maybeProjectRoot <- liftIO $ findProjectRoot file
  workDir <- case maybeProjectRoot of
    Just root -> do
      liftIO $ hPutStrLn stderr $ "Found project root: " ++ root
      liftIO $ absolute root
    Nothing -> do
      liftIO $ hPutStrLn stderr $ "No project root found, using file directory"
      liftIO $ absolute $ takeDirectory file

  -- Discover and load .agda-lib files for this file's project
  _ <- getAgdaLibFilesWithoutTopLevelModuleName absPath

  -- Set up library includes (adds library paths to options)
  let defaultOptsWithLibFile = case libraryFile of
        Just lf -> defaultOptions { optOverrideLibrariesFile = Just (T.unpack lf) }
        Nothing -> defaultOptions
  optsWithLibs <- setLibraryIncludes defaultOptsWithLibFile
  setCommandLineOptions' workDir optsWithLibs

  -- Properly register the file and get its FileId
  fileId <- idFromFile absPath
  let sourceFile = SourceFile fileId
  source <- parseSource sourceFile
  checked <- typeCheckMain TypeCheck source
  setScope $ iInsideScope $ crInterface checked
  -- Update state, preserving channel and thread ID
  state <- liftIO $ readIORef stateRef
  let libFileStr = fmap T.unpack libraryFile
  liftIO $ writeIORef stateRef (state { currentFile = Just absPath
                                      , currentLibraryFile = libFileStr
                                      , checkResult = Just checked })
  pure $ AgdaResult True "File loaded successfully" Nothing

mcpListPostulates :: IORef ServerState -> FilePath -> TCM AgdaResult
mcpListPostulates stateRef filepath = do
  -- Wrap in error handler to provide simplified error messages
  catchError
    (do
      -- Use cached load if available
      checked <- loadFileWithCache stateRef filepath Nothing

      -- Get all definitions from the interface's signature (not global signature)
      let sig = iSignature (crInterface checked)
      let allDefs = HashMap.toList (_sigDefinitions sig)

      -- Filter for axioms (postulates) and extract information
      postulates <- catMaybes <$> forM allDefs (\(qname, def) -> do
        case theDef def of
          Axiom{} -> do
            -- Get type as text
            typ <- prettyTCM (defType def)
            -- Get the name range (position in file)
            let nameRange = nameBindingSite (qnameName qname)
            return $ Just PostulateInfo
              { pName = T.pack $ prettyShow qname
              , pType = T.pack $ render typ
              , pRange = rangeToTuple nameRange
              }
          _ -> return Nothing
        )

      pure $ AgdaResult True "Success" (Just $ JSON.toJSON postulates)
    )
    (\err -> do
      -- Simplify error message - just get the error description without full type details
      errMsg <- prettyTCM err
      let simplifiedMsg = T.pack $ take 500 $ render errMsg  -- Truncate to 500 chars
      pure $ AgdaResult False ("Type-checking failed: " <> simplifiedMsg) Nothing
    )

mcpGoalAtPosition :: IORef ServerState -> FilePath -> Int -> Int -> TCM AgdaResult
mcpGoalAtPosition stateRef filepath line col = do
  -- Wrap in error handler
  catchError
    (do
      -- Use cached load if available
      checked <- loadFileWithCache stateRef filepath Nothing

      -- Get all interaction points (goals)
      interactionIds <- getInteractionPoints

      -- Find the goal at the specified position
      matchingGoal <- findGoalAtPositionTCM interactionIds line col

      case matchingGoal of
        Nothing -> pure $ AgdaResult False "No goal found at specified position" Nothing
        Just (iid, range, goalTypeText) -> do
          let goalInfo = JSON.object
                [ "goalId" JSON..= (\(InteractionId n) -> n) iid
                , "goalType" JSON..= goalTypeText
                , "range" JSON..= rangeToTuple range
                ]
          pure $ AgdaResult True "Success" (Just goalInfo)
    )
    (\err -> do
      errMsg <- prettyTCM err
      let simplifiedMsg = T.pack $ take 500 $ render errMsg
      pure $ AgdaResult False ("Type-checking failed: " <> simplifiedMsg) Nothing
    )
  where
-- Helper for GoalAtPosition (Used in custom handler)
findGoalAtPositionTCM :: [InteractionId] -> Int -> Int -> TCM (Maybe (InteractionId, Range, Text))
findGoalAtPositionTCM [] _ _ = return Nothing
findGoalAtPositionTCM (iid:rest) targetLine targetCol = do
  range <- getInteractionRange iid
  let (startLine, startCol, endLine, endCol) = rangeToTuple range
  -- Check if position falls within this goal's range
  if positionInRange targetLine targetCol startLine startCol endLine endCol
    then do
      constraint <- BasicOps.typeOfMeta AsIs iid
      goalTypeText <- renderConstraintType constraint
      return $ Just (iid, range, goalTypeText)
    else
      findGoalAtPositionTCM rest targetLine targetCol

positionInRange :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
positionInRange targetLine targetCol startLine startCol endLine endCol
  | targetLine < startLine = False
  | targetLine > endLine = False
  | targetLine == startLine && targetCol < startCol = False
  | targetLine == endLine && targetCol > endCol = False
  | otherwise = True

mcpGotoDefinition :: IORef ServerState -> FilePath -> Int -> Int -> TCM AgdaResult
mcpGotoDefinition stateRef filepath line col = do
  -- Wrap in error handler
  catchError
    (do
      -- Use cached load if available
      checked <- loadFileWithCache stateRef filepath Nothing

      -- Extract highlighting directly from the interface after type-checking
      -- This is synchronous and doesn't require waiting for async callbacks
      let interface = crInterface checked
      let highlighting = iHighlighting interface

      liftIO $ hPutStrLn stderr $ "Extracted highlighting directly from interface"

      -- We need to convert HighlightingInfo to JSON
      -- HighlightingInfo is RangeMap Aspects
      -- Let's check what the callback received and log it
      stateAfterLoad <- liftIO $ readIORef stateRef
      case highlightingInfo stateAfterLoad of
        Nothing -> do
          liftIO $ hPutStrLn stderr "No highlighting in callback, but we have it from interface"
          -- For now, return error - need to implement direct conversion
          pure $ AgdaResult False "Highlighting available but conversion not yet implemented. Need to parse RangeMap Aspects directly." Nothing
        Just highlightJson -> do
          liftIO $ do
            let jsonText = TE.decodeUtf8 $ LBS.toStrict $ JSON.encode highlightJson
            hPutStrLn stderr $ "Highlighting JSON from callback (first 2000 chars): " ++ take 2000 (T.unpack jsonText)
          -- Parse highlighting to find definition at position
          case findDefinitionAtPosition highlightJson line col of
            Nothing -> pure $ AgdaResult False "No definition found at specified position. The position may be on whitespace, keywords, or symbols without definition sites." Nothing
            Just defInfo -> pure $ AgdaResult True "Success" (Just defInfo)
    )

    (\err -> do
      errMsg <- prettyTCM err
      let simplifiedMsg = T.pack $ take 500 $ render errMsg
      pure $ AgdaResult False ("Type-checking failed: " <> simplifiedMsg) Nothing
    )

findDefinitionAtPosition :: JSON.Value -> Int -> Int -> Maybe JSON.Value
findDefinitionAtPosition highlightJson line col = do
  -- Extract payload array from highlighting JSON structure
  info <- getJSONField "info" highlightJson
  payload <- getJSONField "payload" info
  payloadArray <- case payload of
    JSON.Array arr -> Just arr
    _ -> Nothing

  -- Search through payload for entry at given position
  findMatchingEntry (V.toList payloadArray) line col

findMatchingEntry :: [JSON.Value] -> Int -> Int -> Maybe JSON.Value
findMatchingEntry [] _ _ = Nothing
findMatchingEntry (entry:rest) targetLine targetCol = do
  -- Check if this entry's range contains the target position
  rangeVal <- getJSONField "range" entry
  (startPos, endPos) <- case rangeVal of
    JSON.Array arr | V.length arr >= 2 ->
      case (arr V.! 0, arr V.! 1) of
        (JSON.Number s, JSON.Number e) -> Just (floor s :: Int, floor e :: Int)
        _ -> Nothing
    _ -> Nothing

  -- Ranges in Agda highlighting are character offsets, not line/col
  -- For now, skip position matching and just look for definitionSite
  defSite <- getJSONField "definitionSite" entry
  case defSite of
    JSON.Null -> findMatchingEntry rest targetLine targetCol
    _ -> Just defSite  -- Found a definition site

getJSONField :: Text -> JSON.Value -> Maybe JSON.Value
getJSONField field (JSON.Object obj) = JSON.KeyMap.lookup (JSON.Key.fromText field) obj
getJSONField _ _ = Nothing

mcpGetGoals :: IORef ServerState -> TCM AgdaResult
mcpGetGoals stateRef = do
  ensureFileLoaded stateRef
  interactionIds <- getInteractionPoints
  goals <- forM interactionIds $ \iid -> do
    range <- getInteractionRange iid
    constraint <- BasicOps.typeOfMeta AsIs iid
    goalTypeText <- renderConstraintType constraint
    return $ AgdaGoal
      { goalId = (\(InteractionId n) -> n) iid
      , goalType = goalTypeText
      , goalRange = rangeToTuple range
      }
  pure $ AgdaResult True "Success" (Just $ JSON.toJSON goals)

mcpGetGoalType :: IORef ServerState -> Int -> TCM AgdaResult
mcpGetGoalType stateRef goalIdParam = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  constraint <- withInteractionId iid $ BasicOps.typeOfMeta AsIs iid
  goalTypeText <- renderConstraintType constraint
  pure $ AgdaResult True "Success" (Just $ JSON.String goalTypeText)

mcpGetContext :: IORef ServerState -> Int -> TCM AgdaResult
mcpGetContext stateRef goalIdParam = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  constraint <- withInteractionId iid $ BasicOps.typeOfMeta AsIs iid
  contextEntries <- renderConstraintContext constraint
  pure $ AgdaResult True "Success" (Just $ JSON.toJSON contextEntries)

mcpGive :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpGive stateRef goalIdParam exprStr = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  expr <- BasicOps.parseExprIn iid noRange exprStr
  result <- withInteractionId iid $ BasicOps.give WithoutForce iid Nothing expr
  resultDoc <- prettyTCM result
  let resultText = render resultDoc
  pure $ AgdaResult True "Expression given" (Just $ JSON.String $ T.pack resultText)

mcpRefine :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpRefine stateRef goalIdParam exprStr = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  expr <- BasicOps.parseExprIn iid noRange exprStr
  result <- withInteractionId iid $ BasicOps.refine WithoutForce iid Nothing expr
  resultDoc <- prettyTCM result
  let resultText = render resultDoc
  pure $ AgdaResult True "Refinement successful" (Just $ JSON.String $ T.pack resultText)

mcpCaseSplit :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpCaseSplit stateRef goalIdParam varName = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  (_, _, clauses) <- withInteractionId iid $ makeCase iid noRange varName
  -- Convert clauses to pretty text
  clauseDocs <- mapM prettyA clauses
  let clauseTexts = map (T.pack . render) clauseDocs
  pure $ AgdaResult True "Case split successful" (Just $ JSON.toJSON clauseTexts)

mcpCompute :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpCompute stateRef goalIdParam exprStr = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  expr <- BasicOps.parseExprIn iid noRange exprStr
  -- For now, just return the parsed expression as we can't normalize Expr directly
  -- We would need to convert to internal syntax first
  doc <- prettyA expr
  let resultText = render doc
  pure $ AgdaResult True "Expression parsed" (Just $ JSON.String $ T.pack resultText)

mcpInferType :: IORef ServerState -> Int -> String -> TCM AgdaResult
mcpInferType stateRef goalIdParam exprStr = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  expr <- BasicOps.parseExprIn iid noRange exprStr
  typeExpr <- withInteractionId iid $ BasicOps.typeInMeta iid AsIs expr
  resultDoc <- prettyTCM typeExpr
  let resultText = render resultDoc
  pure $ AgdaResult True "Type inferred" (Just $ JSON.String $ T.pack resultText)

mcpIntro :: IORef ServerState -> Int -> TCM AgdaResult
mcpIntro stateRef goalIdParam = do
  ensureFileLoaded stateRef
  let iid = InteractionId goalIdParam
  intros <- withInteractionId iid $ BasicOps.introTactic False iid
  pure $ AgdaResult True "Intro suggestions" (Just $ JSON.toJSON $ map T.pack intros)

mcpWhyInScope :: IORef ServerState -> String -> TCM AgdaResult
mcpWhyInScope stateRef name = do
  ensureFileLoaded stateRef
  state <- liftIO $ readIORef stateRef
  let absPath = case currentFile state of
        Just path -> path
        Nothing -> error "ensureFileLoaded should have caught this"
  whyData <- BasicOps.whyInScope (show absPath) name
  let WhyInScopeData qname file _ defs mods = whyData
  let result = JSON.object
        [ "qualified_name" JSON..= T.pack (show qname)
        , "file" JSON..= T.pack file
        , "definitions" JSON..= (map (T.pack . show) defs)
        , "modules" JSON..= (map (T.pack . show) mods)
        ]
  pure $ AgdaResult True "Scope information" (Just result)

-- Helper functions

-- | Find the project root by walking up directories looking for .agda-lib files
-- Returns the directory containing an .agda-lib file, or Nothing if not found
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot startPath = do
  let dir = takeDirectory startPath
  if dir == startPath  -- Reached filesystem root
    then return Nothing
    else do
      -- List all files in directory (return empty list on error)
      filesResult <- Control.Exception.try (listDirectory dir) :: IO (Either Control.Exception.SomeException [FilePath])
      let files = case filesResult of
            Left _ -> []
            Right fs -> fs
      -- Check if any file ends with .agda-lib
      let hasAgdaLib = any (\f -> ".agda-lib" `Data.List.isSuffixOf` f) files
      if hasAgdaLib
        then return (Just dir)
        else findProjectRoot dir

rangeToTuple :: Range -> (Int, Int, Int, Int)
rangeToTuple range =
  case (rStart range, rEnd range) of
    (Just startPos, Just endPos) ->
      (fromIntegral (posLine startPos), fromIntegral (posCol startPos),
       fromIntegral (posLine endPos), fromIntegral (posCol endPos))
    _ -> (0, 0, 0, 0)

renderConstraintType :: OutputConstraint Expr InteractionId -> TCM Text
renderConstraintType constraint = do
  -- Extract the type from the constraint and render it
  doc <- prettyA constraint
  return $ T.pack $ render doc

renderConstraintContext :: OutputConstraint Expr InteractionId -> TCM [AgdaContextEntry]
renderConstraintContext _constraint = do
  -- For now, return empty context - we'll need to dig into the constraint structure
  -- to extract context entries. This would require pattern matching on the OutputConstraint
  -- which contains the context information.
  return []

-- ============================================================================
-- MCP Library Integration
-- ============================================================================

-- Initialize server state and start persistent REPL
initServerState :: IO (IORef ServerState)
initServerState = do
  chan <- newChan
  responseVarRef <- newIORef Nothing
  shutdownVar <- newEmptyMVar
  stateRef <- newIORef (ServerState Nothing Nothing Nothing Nothing chan Nothing shutdownVar responseVarRef)

  -- Start persistent REPL in background thread using async
  asyncHandle <- async $ do
    hPutStrLn stderr "Starting persistent Agda REPL thread..."
    result <- runTCMTop $ Repl.mcpRepl
      (mcpCallback stateRef responseVarRef)          -- Response callback with state and response refs
      (readCommandFromChan responseVarRef shutdownVar chan) -- Command source with shutdown support
      (return ())                                    -- No special setup
    case result of
      Left err -> hPutStrLn stderr $ "REPL error: " ++ show err
      Right _ -> hPutStrLn stderr "REPL exited normally"

  -- Update state with async handle
  modifyIORef stateRef (\s -> s { replAsync = Just asyncHandle })
  hPutStrLn stderr "Persistent REPL started"
  pure stateRef

-- MCP Tool Handler - routes commands through persistent REPL
-- This will be called by the mcp-server library when a tool is invoked
handleAgdaTool :: IORef ServerState -> AgdaMCP.Types.AgdaTool -> IO MCP.Server.Content
handleAgdaTool stateRef tool = do
  state <- readIORef stateRef

  -- Get current file path for IOTCM commands
  let currentFilePath = case currentFile state of
        Just absPath -> filePath absPath
        Nothing -> ""

  case tool of
    AgdaMCP.Types.AgdaListPostulates{file} -> do
      responseVar <- newEmptyMVar
      
      -- Custom handler to extract postulates from interface
      let handler resp = case resp of
            -- We expect Resp_DoneExiting after Cmd_load, but we can also inspect the state
            Resp_DoneExiting -> do
              -- File should be loaded and checked by now.
              -- We can now call mcpListPostulates directly in TCM context.
              -- This will use the cached CheckResult from the state.
              result <- mcpListPostulates stateRef (T.unpack file)
              case result of
                AgdaResult _ _ (Just val) -> return $ Just val
                _ -> return $ Just $ JSON.object ["error" JSON..= ("Failed to list postulates: " <> message result)]
            _ -> return Nothing 
            
      let iotcm = toolToIOTCM currentFilePath tool
      let cmd = CommandWithResponse iotcm responseVar (Just handler)
      writeChan (commandChan state) cmd
      hPutStrLn stderr $ "Sent ListPostulates command to REPL"
      jsonValue <- takeMVar responseVar
      hPutStrLn stderr "Received encoded response for ListPostulates from REPL"
      let responseText = Format.formatResponse (Format.getFormat tool) jsonValue
      pure $ MCP.Server.ContentText responseText

    AgdaMCP.Types.AgdaGoalAtPosition{file, line, column} -> do
      responseVar <- newEmptyMVar
      
      -- Handler for GoalAtPosition
      -- We expect Resp_InteractionPoints when using Cmd_metas
      let handler resp = case resp of
            Resp_InteractionPoints _ -> do
               -- We are in TCM! We can look up goal at position
               -- We don't rely on the IDs in the message, just query local state
               interactionIds <- getInteractionPoints
               -- Reuse the findGoalAtPosition logic but lifted to TCM directly
               matchingGoal <- findGoalAtPositionTCM interactionIds line column
               
               case matchingGoal of
                 Nothing -> return $ Just $ JSON.object 
                   [ "error" JSON..= ("No goal found at position " ++ show line ++ ":" ++ show column :: String) ]
                 Just (iid, range, goalTypeText) -> do
                   let goalInfo = JSON.object
                         [ "goalId" JSON..= (\(InteractionId n) -> n) iid
                         , "goalType" JSON..= goalTypeText
                         , "range" JSON..= rangeToTuple range
                         ]
                   return $ Just goalInfo
            _ -> return Nothing

      let iotcm = toolToIOTCM currentFilePath tool
      let cmd = CommandWithResponse iotcm responseVar (Just handler)
      
      writeChan (commandChan state) cmd
      hPutStrLn stderr $ "Sent GoalAtPosition command to REPL"

      jsonValue <- takeMVar responseVar
      hPutStrLn stderr "Received response for GoalAtPosition"
      
      let responseText = Format.formatResponse (Format.getFormat tool) jsonValue
      pure $ MCP.Server.ContentText responseText

    AgdaMCP.Types.AgdaGotoDefinition{file, line, column} -> do
      responseVar <- newEmptyMVar
      
      -- Handler for GotoDefinition
      let handler resp = case resp of
            -- We expect Resp_HighlightingInfo after Cmd_load
            Resp_HighlightingInfo _ _ _ _ -> do
              -- Highlighting info should now be in stateRef
              stateAfterLoad <- liftIO $ readIORef stateRef
              case highlightingInfo stateAfterLoad of
                Nothing -> return $ Just $ JSON.object ["error" JSON..= ("No highlighting info found after load" :: String)]
                Just highlightJson -> do
                  case findDefinitionAtPosition highlightJson line column of
                    Nothing -> return $ Just $ JSON.object ["error" JSON..= ("No definition found at specified position." :: String)]
                    Just defInfo -> return $ Just defInfo
            _ -> return Nothing

      let iotcm = toolToIOTCM currentFilePath tool
      let cmd = CommandWithResponse iotcm responseVar (Just handler)
      
      writeChan (commandChan state) cmd
      hPutStrLn stderr $ "Sent GotoDefinition command to REPL"
      jsonValue <- takeMVar responseVar
      hPutStrLn stderr "Received encoded response for GotoDefinition from REPL"
      
      let responseText = Format.formatResponse (Format.getFormat tool) jsonValue
      pure $ MCP.Server.ContentText responseText

    -- All other tools go through REPL with no custom handler
    _ -> do
      -- Convert tool to IOTCM command
      let iotcm = toolToIOTCM currentFilePath tool

      -- Create response MVar and command wrapper
      responseVar <- newEmptyMVar
      let cmd = CommandWithResponse iotcm responseVar Nothing

      -- Send command to persistent REPL
      writeChan (commandChan state) cmd
      hPutStrLn stderr $ "Sent command to REPL: " ++ show tool

      -- Wait for encoded JSON response from REPL
      jsonValue <- takeMVar responseVar
      hPutStrLn stderr "Received encoded response from REPL"

      -- If this was a successful load, update the current file in state
      case tool of
        AgdaMCP.Types.AgdaLoad{file, libraryFile = _} -> do
          absPath <- absolute (T.unpack file)
          modifyIORef stateRef (\s -> s { currentFile = Just absPath })
          hPutStrLn stderr $ "Updated current file to: " ++ T.unpack file
        _ -> return ()

      -- Format response based on requested format (default: Concise)
      let responseFormat = Format.getFormat tool
      let responseText = Format.formatResponse responseFormat jsonValue
      pure $ MCP.Server.ContentText responseText

-- MCP Resource Handler - exposes Agda file information as resources
-- Resources extract parameters from the URI path
handleAgdaResource :: IORef ServerState -> MCP.Server.URI -> AgdaMCP.Types.AgdaResource -> IO MCP.Server.ResourceContent
handleAgdaResource stateRef uri resource = do
  result <- runTCMTop $ case resource of
    AgdaMCP.Types.Goals -> do
      -- URI format: resource://goals/{file}
      -- For now, just get goals from currently loaded file
      mcpGetGoals stateRef

    AgdaMCP.Types.GoalInfo -> do
      -- URI format: resource://goal_info/{file}/{id}
      -- For now, extract goal ID from URI path (simplified - would need proper URI parsing)
      -- Just return error for now as this requires URI path parsing
      pure $ AgdaResult False "GoalInfo resource not yet implemented - use agda_get_goal_type tool instead" Nothing

    AgdaMCP.Types.FileContext ->
      -- URI format: resource://file_context/{file}
      -- Return file context info
      pure $ AgdaResult True "File context" Nothing

  case result of
    Left err ->
      pure $ MCP.Server.ResourceText uri "text/plain" $ "Error: " <> T.pack (show err)
    Right agdaRes ->
      case agdaResult agdaRes of
        Nothing ->
          pure $ MCP.Server.ResourceText uri "text/plain" $ message agdaRes
        Just val ->
          pure $ MCP.Server.ResourceText uri "application/json" $ TE.decodeUtf8 $ LBS.toStrict $ JSON.encode val

-- ============================================================================
-- Session-based Handlers (Multi-agent Support)
-- ============================================================================

-- | Initialize session manager instead of single server state
-- This is the new entry point for Main.hs
initSessionManager :: IO (SessionManager.SessionManager ServerState)
initSessionManager = do
  -- Create session manager with initServerState as the state initializer
  -- and a cleanup function that gracefully shuts down the REPL thread
  let cleanup stateRef = do
        state <- readIORef stateRef
        case replAsync state of
          Just asyncHandle -> do
            hPutStrLn stderr "Gracefully shutting down REPL thread..."
            -- Signal shutdown via MVar (tryPutMVar won't block if already signaled)
            void $ tryPutMVar (shutdownVar state) ()
            -- Wait for thread to actually exit (max 5 seconds)
            result <- timeout 5000000 $ waitCatch asyncHandle
            case result of
              Nothing -> hPutStrLn stderr "Warning: REPL thread did not exit within 5 seconds"
              Just (Left ex) -> hPutStrLn stderr $ "REPL thread exited with exception: " ++ show ex
              Just (Right ()) -> hPutStrLn stderr "REPL thread shutdown complete"
          Nothing -> return ()

  SessionManager.createSessionManager initServerState (Just cleanup)

-- | Session-aware tool handler
-- Extracts sessionId from tool, gets or creates session, routes to appropriate ServerState
handleAgdaToolWithSession :: SessionManager.SessionManager ServerState -> AgdaMCP.Types.AgdaTool -> IO MCP.Server.Content
handleAgdaToolWithSession manager tool = do
  -- Extract session ID from tool (all tools now have this field)
  let sessionId = AgdaMCP.Types.sessionId tool

  -- Get or create session
  stateRef <- SessionManager.getOrCreateSession manager sessionId

  -- Update last used timestamp
  SessionManager.updateLastUsed manager sessionId

  -- Route to existing handler
  handleAgdaTool stateRef tool

-- | Session-aware resource handler
handleAgdaResourceWithSession :: SessionManager.SessionManager ServerState -> MCP.Server.URI -> AgdaMCP.Types.AgdaResource -> IO MCP.Server.ResourceContent
handleAgdaResourceWithSession manager uri resource = do
  -- Resources don't have sessionId parameter, so use default session
  -- In the future, we could extract session from URI query parameters
  stateRef <- SessionManager.getOrCreateSession manager Nothing
  handleAgdaResource stateRef uri resource
