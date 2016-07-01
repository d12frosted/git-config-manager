{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

--------------------------------------------------------------------------------
-- * Internal imports

import           Git
import           Types

--------------------------------------------------------------------------------
-- * External imports

import           Control.Monad.Catch (MonadThrow (..))
import           Data.HashMap.Strict as Map
import           Data.Text           (pack)
import qualified Data.Text.IO        as T (putStr, putStrLn)
import           Options.Applicative
import           Prelude

--------------------------------------------------------------------------------
-- * Data types

data AppOptions = AppOptions { optVerbose :: Bool
                             , optFile    :: Maybe String
                             , optCommand :: AppCmd }
                 deriving Show

data AppCmd = AppCmdList |
              AppCmdGet |
              AppCmdSet String |
              AppCmdUnset String
            deriving Show

--------------------------------------------------------------------------------
-- * Application

main :: IO ()
main = execParser opts >>= run

run :: AppOptions -> IO ()
run (AppOptions verbose fileStrM cmd) =
  do path <- Git.getConfigPath fileStrM
     gitConfig <- Git.loadConfig path
     runCmd cmd (AppConfig verbose path) gitConfig

runCmd :: AppCmd -> AppConfig -> GitConfig -> IO ()
runCmd AppCmdList _ (GitConfig cfg) = mapM_ T.putStrLn $ keys cfg
runCmd AppCmdGet _ _ = Git.get "gcm" "scheme" >>= T.putStr
runCmd (AppCmdSet scheme) appCfg cfg =
  mapScheme appCfg scheme cfg $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . Git.set) cfgs
       addScheme . pack $ scheme
runCmd (AppCmdUnset scheme) appCfg cfg =
  mapScheme appCfg scheme cfg $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . (-$) Git.unset) cfgs
       Git.removeScheme . pack $ scheme

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
               command "get" (info (pure AppCmdGet) (progDesc "Get comma-separated list of currently used schemes")) <>
               command "set" (info (AppCmdSet <$> argument str (metavar "SCHEME")) (progDesc "Set up configurations by scheme")) <>
               command "unset" (info (AppCmdUnset <$> argument str (metavar "SCHEME")) (progDesc "Unset configurations by scheme")))

--------------------------------------------------------------------------------
-- * Helpers

lookupScheme :: String -> GitConfig -> Maybe ConfigMap
lookupScheme scheme (GitConfig cfg) = Map.lookup (pack scheme) cfg

mapScheme :: AppConfig -> String -> GitConfig -> (ConfigMap -> IO a) -> IO a
mapScheme appCfg scheme cfg f =
  case lookupScheme scheme cfg of
    Nothing -> throwM $ GCMSchemeNotFound (appConfigFile appCfg) scheme
    Just cfgs -> f cfgs

(-$) :: (a -> b -> d) -> a -> b -> c -> d
f -$ a = \b _ -> f a b

infixl 8 -$
