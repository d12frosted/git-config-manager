{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Git ( GitConfig (..)
           , SchemeMap
           , ConfigMap
           , setConfig
           , unsetConfig
           , getConfig
           , addScheme
           , removeScheme
           , loadConfig
           , getConfigPath
           ) where

--------------------------------------------------------------------------------
-- * Internal imports

import           Types

--------------------------------------------------------------------------------
-- * External imports

import           Control.Applicative  ((<|>))
import           Control.Monad.Catch  (MonadThrow (..))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Attoparsec.Text
import           Data.ByteString.Lazy (readFile)
import           Data.HashMap.Strict  ()
import           Data.HashMap.Strict  as Map
import           Data.HashSet         ()
import           Data.HashSet         as Set
import           Data.Monoid
import           Data.String          (IsString (..))
import           Data.Text            (Text, intercalate, pack, unpack)
import           MTLPrelude
import           Path.Parse
import           Prelude              hiding (print, putStr, putStrLn, readFile)
import qualified Turtle

--------------------------------------------------------------------------------
-- * Data types

type ConfigMap = (HashMap Text Object)
type SchemeMap = HashMap Text ConfigMap
data GitConfig = GitConfig SchemeMap deriving Show

$(deriveJSON defaultOptions ''GitConfig)

--------------------------------------------------------------------------------
-- * Operations

setConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> Value -> m ()
setConfig section key val =
  case val of
    Null -> unsetConfig section key
    _ -> case extractConfig val of
      Just cfg -> configProcs [section <> "." <> key, cfg]
      Nothing  -> throwM $ GCMConfigTypeNotSupported (unpack section) (unpack key) val

unsetConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> m ()
unsetConfig section key =
  do configProcs ["--unset", section <> "." <> key]
     (res, _) <- Turtle.procStrict "git" ["config", "--get-regexp", "--local", "^" <> section <> "\\."] Turtle.empty
     case res of
       Turtle.ExitFailure 1 -> configProcs ["--remove-section", section]
       _ -> return ()

getConfig :: (MonadIO m) => Text -> Text -> m Text
getConfig section key = snd <$> Turtle.procStrict "git" ["config", section <> "." <> key] Turtle.empty

--------------------------------------------------------------------------------
-- ** Scheme

getSchemes :: (MonadThrow m, MonadIO m) => m (HashSet Text)
getSchemes =
  do raw <- getConfig "gcm" "scheme"
     case parseOnly pSchemes raw of
       Right val -> return $ Set.fromList val
       Left err -> throwM $ GCMSchemesParseError err

addScheme :: (MonadThrow m, MonadIO m) => Text -> m ()
addScheme scheme =
  do schemes <- getSchemes
     setSchemes $ Set.insert scheme schemes

removeScheme :: (MonadThrow m, MonadIO m) => Text -> m ()
removeScheme scheme =
  do schemes <- getSchemes
     setSchemes $ Set.delete scheme schemes

setSchemes :: (MonadThrow m, MonadIO m) => HashSet Text -> m ()
setSchemes = setConfig "gcm" "scheme" . String . intercalate ", " . Set.toList

pScheme :: Parser Text
pScheme = pack <$> many1 (letter <|> digit)

pSchemes :: Parser [Text]
pSchemes = pScheme `sepBy` sep
  where sep = many' space >> char ',' >> many' space

--------------------------------------------------------------------------------
-- * Loading

loadConfig :: (MonadThrow m, MonadIO m) => Path Abs File -> m GitConfig
loadConfig path =
  do contents <- liftIO . readFile . toFilePath $ path
     case eitherDecode' contents of
       Left msg -> throwM $ GCMParseError path msg
       Right cfg -> return cfg

getConfigPath :: (MonadThrow m, MonadIO m) => Maybe Text -> m (Path Abs File)
getConfigPath pathM =
  case pathM of
    Just path -> parseFilePath path
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
