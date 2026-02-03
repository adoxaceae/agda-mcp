{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import MCP.Server
import MCP.Server.Derive
import System.IO (hSetEncoding, stderr, stdout, utf8, hPutStrLn)

import AgdaMCP.Server
import qualified AgdaMCP.SessionManager as SessionManager
import AgdaMCP.Types

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  
  -- Set UTF-8 encoding for proper Unicode handling
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  -- Initialize session manager (replaces single server state)
  sessionManager <- initServerStateWithManager

  -- Define handlers that close over sessionManager
  let handleTool :: AgdaTool -> IO Content
      handleTool = handleAgdaToolWithSession sessionManager

      handleResource :: URI -> AgdaResource -> IO ResourceContent
      handleResource = handleAgdaResourceWithSession sessionManager

      -- Derive MCP handlers using Template Haskell
      tools = $(deriveToolHandlerWithDescription ''AgdaTool 'handleTool agdaToolDescriptions)
      resources = $(deriveResourceHandlerWithDescription ''AgdaResource 'handleResource agdaResourceDescriptions)
      
      serverInfo = McpServerInfo
          { serverName = "Agda MCP Server"
          , serverVersion = "1.0.0"
          , serverInstructions = "A Model Context Protocol server for interactive Agda development. Provides tools for loading files, working with goals/holes, refining proofs, and exploring scope. Supports multi-agent isolation via sessionId parameter."
          }
      serverHandlers = McpServerHandlers
          { prompts = Nothing
          , resources = Just resources
          , tools = Just tools
          }

  if "--stdio" `elem` args
    then do
      hPutStrLn stderr "Starting Agda MCP Server on stdio"
      runMcpServerStdio serverInfo serverHandlers
    else do
      let port = 3000 -- Could also parse from args if needed
      hPutStrLn stderr $ "Starting Agda MCP Server on http://localhost:" ++ show port ++ "/mcp"
      runMcpServerHttp serverInfo serverHandlers

-- Wrapper to initialize the server state and return the session manager
initServerStateWithManager :: IO (SessionManager.SessionManager ServerState)
initServerStateWithManager = initSessionManager
