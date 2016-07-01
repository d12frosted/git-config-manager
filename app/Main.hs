{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main
            , AppOptions(..)
            ) where

--------------------------------------------------------------------------------
-- * Internal imports

import           Git
import           Types

--------------------------------------------------------------------------------
-- * External imports

import           BasePrelude         hiding (putStrLn)
import           Control.Monad.Catch (MonadThrow (..))
import           Data.HashMap.Strict as Map
import           Data.HashSet        as Set
import           Data.Text           (Text, pack, unpack)
import           Data.Text.IO        (putStrLn)
import           MTLPrelude
import           Options.Applicative

--------------------------------------------------------------------------------
-- * Data types

data AppOptions = AppOptions { optVerbose :: Bool
                             , optFile    :: Maybe Text
                             , optCommand :: AppCmd }
                 deriving Show

data AppCmd = AppCmdList |
              AppCmdGet |
              AppCmdSet Text |
              AppCmdUnset Text
            deriving Show

--------------------------------------------------------------------------------
-- * Application

main :: IO ()
main = execParser opts >>= run

run :: AppOptions -> IO ()
run (AppOptions verbose pathM cmd) =
  do path <- getConfigPath pathM
     cfg <- loadConfig path
     runReaderT (runCmd cmd) (AppConfig verbose path cfg)

runCmd :: AppCmd -> AppIO ()
runCmd AppCmdList = askGitConfig >>= lift . mapM_ putStrLn . keys
runCmd AppCmdGet = getSchemes >>= lift . mapM_ putStrLn . Set.toList
runCmd (AppCmdSet scheme) =
  mapScheme scheme $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . setConfig) cfgs
       addScheme scheme
runCmd (AppCmdUnset scheme) =
  mapScheme scheme $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . (-$) unsetConfig) cfgs
       removeScheme scheme

--------------------------------------------------------------------------------
-- * Parsers

opts :: ParserInfo AppOptions
opts = info (helper <*> configParser)
  (fullDesc <> progDesc "Manage git configurations and switch between them with ease" <> header "Git configuration manager")

configParser :: Parser AppOptions
configParser =
  AppOptions <$>
    switch (long "verbose" <> help "Enable verbose mode") <*>
    optional (txtOption $ long "config-file" <> metavar "PATH" <> help "Specify configuration file path") <*>
    subparser (command "list" (info (pure AppCmdList) (progDesc "List all available configuration schemes")) <>
               command "get" (info (pure AppCmdGet) (progDesc "Get list of currently used schemes")) <>
               command "set" (info (AppCmdSet . pack <$> argument str (metavar "SCHEME")) (progDesc "Set up configurations by scheme")) <>
               command "unset" (info (AppCmdUnset . pack <$> argument str (metavar "SCHEME")) (progDesc "Unset configurations by scheme")))

--------------------------------------------------------------------------------
-- * Helpers

lookupScheme :: (Monad m) => Text -> AppT m (Maybe ConfigMap)
lookupScheme scheme = liftM (Map.lookup scheme) askGitConfig

mapScheme :: Text -> (ConfigMap -> AppIO a) -> AppIO a
mapScheme scheme f =
  lookupScheme scheme >>= \case
    Nothing -> askConfigPath >>= throwM . GCMSchemeNotFound (unpack scheme)
    Just cfgs -> f cfgs

(-$) :: (a -> b -> d) -> a -> b -> c -> d
f -$ a = \b _ -> f a b

infixl 8 -$

txtOption :: Mod OptionFields String -> Parser Text
txtOption = fmap pack . strOption
