{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main (main) where

--------------------------------------------------------------------------------
-- * Internal imports

import Types
import Git

--------------------------------------------------------------------------------
-- * External imports

import           Control.Monad.Catch    (MonadThrow (..))
import           Data.HashMap.Strict    as Map
import           Data.Text              (pack)
import qualified Data.Text.IO           as T (putStr, putStrLn)
import           Options.Applicative
import           Prelude

--------------------------------------------------------------------------------
-- * Data types

data AppOptions = AppOptions { optVerbose :: Bool
                             , optFile    :: Maybe String
                             , optCommand :: AppCmd }
                 deriving Show

data AppCmd = AppCmdList | AppCmdGet | AppCmdSet String deriving Show

--------------------------------------------------------------------------------
-- * Application

main :: IO ()
main = execParser opts >>= run

run :: AppOptions -> IO ()
run (AppOptions verbose fileStrM cmd) =
  do path <- getGitConfigPath fileStrM
     gitConfig <- loadGitConfig path
     runCmd cmd (AppConfig verbose path) gitConfig

runCmd :: AppCmd -> AppConfig -> GitConfig -> IO ()
runCmd AppCmdList _ (GitConfig cfg) = mapM_ T.putStrLn $ keys cfg
runCmd AppCmdGet _ _ = getGitConfig "gcm" "scheme" >>= T.putStr
runCmd (AppCmdSet scheme) (AppConfig _ path) (GitConfig cfg) =
  case Map.lookup (pack scheme) cfg of
    Nothing -> throwM $ GCMSchemeNotFound path scheme
    Just cfgs ->
      do _ <- traverseWithKey (traverseWithKey . setGitConfig) cfgs
         setGitConfig "gcm" "scheme" (String . pack $ scheme)

--------------------------------------------------------------------------------
-- * Parsers

opts :: ParserInfo AppOptions
opts = info (helper <*> configParser)
  (fullDesc <> progDesc "Manage git configurations and switch between them with ease" <> header "Git configuration manager")

configParser :: Parser AppOptions
configParser =
  AppOptions <$>
    switch (long "verbose" <> help "Enable verbose mode") <*>
    optional (strOption $ long "config-file" <> metavar "PATH" <> help "Specify configuration file path") <*>
    subparser (command "list" (info (pure AppCmdList) (progDesc "List all available configuration schemes")) <>
               command "get" (info (pure AppCmdGet) (progDesc "Get name of currently used scheme")) <>
               command "set" (info (AppCmdSet <$> argument str (metavar "SCHEME")) (progDesc "Set up configurations by scheme")))
