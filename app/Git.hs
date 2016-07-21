{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Git ( setConfig
           , unsetConfig
           , getConfig
           , setScheme
           , unsetScheme
           , getSchemes
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

import           BasePrelude          hiding (print, putStr, putStrLn, readFile)
import           Control.Monad.Catch  (MonadThrow (..))
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.ByteString.Lazy (readFile)
import           Data.HashMap.Strict  as Map
import           Data.HashSet         ()
import           Data.HashSet         as Set
import           Data.Text            as Text (Text, intercalate, pack, unpack)
import           MTLPrelude
import           Path.Parse
import qualified Turtle
import           Turtle ((.&&.))

--------------------------------------------------------------------------------
-- * Operations

setConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> Value -> AppT m ()
setConfig section key val =
  case val of
    Null -> unsetConfig section key
    _ -> case extractConfig val of
      Just cfg -> procs [mkKey section key, cfg]
      Nothing  -> throwM $ GCMConfigTypeNotSupported (unpack section) (unpack key) val

unsetConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> AppT m ()
unsetConfig section key = void $
  proc ["--unset",mkKey section key] .&&.
  expectCode (ExitFailure 1) (proc ["--get-regexp", "--local", "^" <> section <> "\\."]) .&&.
  proc ["--remove-section", section]

getConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> AppT m Text
getConfig section key = snd <$> procStrict [mkKey section key]

--------------------------------------------------------------------------------
-- ** Scheme

setScheme :: (MonadThrow m, MonadIO m) => Text -> AppT m ()
setScheme scheme =
  mapScheme scheme $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . setConfig) cfgs
       addScheme scheme

unsetScheme :: (MonadThrow m, MonadIO m) => Text -> AppT m ()
unsetScheme scheme =
  mapScheme scheme $ \cfgs ->
    do _ <- traverseWithKey (traverseWithKey . (-$) unsetConfig) cfgs
       removeScheme scheme

getSchemes :: (MonadThrow m, MonadIO m) => AppT m (HashSet Text)
getSchemes = getConfig "gcm" "scheme" >>=
  either (throwM . GCMSchemesParseError) (return . Set.fromList) . parseOnly pSchemes

addScheme :: (MonadThrow m, MonadIO m) => Text -> AppT m ()
addScheme scheme =
  do schemes <- getSchemes
     setSchemes $ Set.insert scheme schemes

removeScheme :: (MonadThrow m, MonadIO m) => Text -> AppT m ()
removeScheme scheme =
  do schemes <- getSchemes
     setSchemes $ Set.delete scheme schemes

setSchemes :: (MonadThrow m, MonadIO m) => HashSet Text -> AppT m ()
setSchemes set =
  if Set.null set
  then unsetConfig "gcm" "scheme"
  else setConfig "gcm" "scheme" . String . Text.intercalate ", " . Set.toList $ set

pScheme :: Parser Text
pScheme = pack <$> many1 (letter <|> digit)

pSchemes :: Parser [Text]
pSchemes = pScheme `sepBy` sep
  where sep = many' space >> char ',' >> many' space

--------------------------------------------------------------------------------
-- * Loading

loadConfig :: (MonadThrow m, MonadIO m) => Path Abs File -> m GitConfig
loadConfig path = (liftIO . readFile . toFilePath $ path) >>=
  either (throwM . GCMParseError path) return . eitherDecode'

getConfigPath :: (MonadThrow m, MonadIO m) => Maybe Text -> m (Path Abs File)
getConfigPath = maybe getDefaultConfigPath parseFilePath

getDefaultConfigPath :: (MonadThrow m, MonadIO m) => m (Path Abs File)
getDefaultConfigPath = parseFilePath "$XDG_CONFIG_HOME/git/git-config-manager.json"

--------------------------------------------------------------------------------
-- * Helpers

mkKey :: Text -> Text -> Text
mkKey s k = s <> "." <> k

proc :: (MonadThrow m, MonadIO m) => [Text] -> m ExitCode
proc = fmap fst . procStrict

procs :: (MonadThrow m, MonadIO m) => [Text] -> m ()
procs = void . procStrict

procStrict :: (MonadThrow m, MonadIO m) => [Text] -> m (ExitCode, Text)
procStrict args = Turtle.procStrict "git" ("config" : args) Turtle.empty

expectCode :: (Monad m) => ExitCode -> m ExitCode -> m ExitCode
expectCode expected action = trans <$> action
  where trans code = if code == expected then ExitSuccess else code

extractConfig :: Value -> Maybe Text
extractConfig (String val) = Just val
extractConfig (Number val) = Just . pack . show $ val
extractConfig (Bool val) = Just . prettyBool $ val
extractConfig _ = Nothing

prettyBool :: IsString a => Bool -> a
prettyBool True = "true"
prettyBool False = "false"

lookupScheme :: (Monad m) => Text -> AppT m (Maybe ConfigMap)
lookupScheme scheme = liftM (Map.lookup scheme) askGitConfig

mapScheme :: (MonadThrow m, MonadIO m) => Text -> (ConfigMap -> AppT m a) -> AppT m a
mapScheme scheme f = lookupScheme scheme >>= maybe complain f
  where complain = askConfigPath >>= throwM . GCMSchemeNotFound (unpack scheme)

(-$) :: (a -> b -> d) -> a -> b -> c -> d
f -$ a = \b _ -> f a b

infixl 8 -$
