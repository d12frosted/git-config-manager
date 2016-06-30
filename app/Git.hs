{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Git ( GitConfig (..)
           , set
           , unset
           , get
           , loadConfig
           , getConfigPath
           ) where

--------------------------------------------------------------------------------
-- * Internal imports

import           Types

--------------------------------------------------------------------------------
-- * External imports

import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy   (readFile)
import           Data.HashMap.Strict    as Map
import           Data.String            (IsString (..))
import           Data.Text              (Text, pack, unpack)
import           Options.Applicative
import           Path.Parse
import           Prelude                hiding (print, putStr, putStrLn,
                                         readFile)
import qualified Turtle

--------------------------------------------------------------------------------
-- * Data types

type ConfigMap = (Map.HashMap Text Object)
type SchemeMap = Map.HashMap Text ConfigMap
data GitConfig = GitConfig SchemeMap deriving Show

$(deriveJSON defaultOptions ''GitConfig)

--------------------------------------------------------------------------------
-- * Operations

set :: (MonadThrow m, MonadIO m) => Text -> Text -> Value -> m ()
set section key val =
  case val of
    Null -> unset section key
    _ -> case extractConfig val of
      Just cfg -> configProcs [section <> "." <> key, cfg]
      Nothing  -> throwM $ GCMConfigTypeNotSupported (unpack section) (unpack key) val

unset :: (MonadThrow m, MonadIO m) => Text -> Text -> m ()
unset section key =
  do configProcs ["--unset", section <> "." <> key]
     (res, _) <- Turtle.procStrict "git" ["config", "--get-regexp", "^" <> section <> "\\."] Turtle.empty
     case res of
       Turtle.ExitFailure 1 -> configProcs ["--remove-section", section]
       _ -> return ()

get :: (MonadIO m) => Text -> Text -> m Text
get section key = snd <$> Turtle.procStrict "git" ["config", section <> "." <> key] Turtle.empty

--------------------------------------------------------------------------------
-- * Loading

loadConfig :: (MonadThrow m, MonadIO m) => Path Abs File -> m GitConfig
loadConfig path =
  do contents <- liftIO . readFile . toFilePath $ path
     case eitherDecode' contents of
       Left msg -> throwM $ GCMParseError path msg
       Right cfg -> return cfg

getConfigPath :: (MonadThrow m, MonadIO m) => Maybe String -> m (Path Abs File)
getConfigPath fileStrM =
  case fileStrM of
    Just fileStr -> parseFilePath . pack $ fileStr
    Nothing -> getDefaultConfigPath

getDefaultConfigPath :: (MonadThrow m, MonadIO m) => m (Path Abs File)
getDefaultConfigPath = parseFilePath "$XDG_CONFIG_HOME/git/git-config-manager.json"

--------------------------------------------------------------------------------
-- * Helpers

configProcs :: (MonadThrow m, MonadIO m) => [Text] -> m ()
configProcs args = Turtle.procs "git" ("config" : args) Turtle.empty

extractConfig :: Value -> Maybe Text
extractConfig (String val) = Just val
extractConfig (Number val) = Just . pack . show $ val
extractConfig (Bool val) = Just . prettyBool $ val
extractConfig _ = Nothing

prettyBool :: IsString a => Bool -> a
prettyBool True = "true"
prettyBool False = "false"
