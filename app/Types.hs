{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Types ( AppConfig (..)
             , App
             , AppIO
             , AppT
             , GitConfig
             , SchemeMap
             , ConfigMap
             , GCMException(..)
             , Value(..)
             , askGitConfig
             , askConfigPath
             , askVerbosity
             ) where

--------------------------------------------------------------------------------
-- * Internal imports

--------------------------------------------------------------------------------
-- * External imports

import           BasePrelude
import           Data.Aeson
import           Data.Aeson.TH
import           Data.HashMap.Strict (HashMap)
import           Data.Text           (Text)
import           MTLPrelude
import           Path.Parse

--------------------------------------------------------------------------------
-- * Data types

data AppConfig = AppConfig { appVerbose    :: Bool
                           , appConfigFile :: Path Abs File
                           , appGitConfig  :: GitConfig
                           } deriving Show

type App a = AppT Identity a
type AppIO a = AppT IO a
type AppT m a = ReaderT AppConfig m a

askGitConfig :: (Monad m) => ReaderT AppConfig m SchemeMap
askGitConfig = asks (getConfig . appGitConfig)
  where getConfig (GitConfig cfg) = cfg

askConfigPath :: (Monad m) => ReaderT AppConfig m (Path Abs File)
askConfigPath = asks appConfigFile

askVerbosity :: (Monad m) => ReaderT AppConfig m Bool
askVerbosity = asks appVerbose

--------------------------------------------------------------------------------
-- ** Git

type ConfigMap = (HashMap Text Object)
type SchemeMap = HashMap Text ConfigMap
newtype GitConfig = GitConfig SchemeMap deriving Show

$(deriveJSON defaultOptions ''GitConfig)

--------------------------------------------------------------------------------
-- ** Exceptions

data GCMException =
  GCMParseError (Path Abs File) String |
  GCMSchemeNotFound String (Path Abs File) |
  GCMConfigTypeNotSupported String String Value |
  GCMSchemesParseError String
  deriving (Typeable)

instance Exception GCMException

instance Show GCMException where
  show (GCMParseError path msg) =
    "Could not parse git configurations from '" ++ toFilePath path ++ "', with error: " ++ msg
  show (GCMSchemeNotFound scheme path) =
    "Could not find scheme '" ++ scheme ++ "' in '" ++ toFilePath path ++ "'"
  show (GCMConfigTypeNotSupported section key val) =
    "Found value of unsupported type '" ++ showValueType val ++ "' at '" ++ section ++ "." ++ key ++ "'"
  show (GCMSchemesParseError err) =
    "Could not parse schemes with error: " ++ err

--------------------------------------------------------------------------------
-- * Helpers

showValueType :: IsString a => Value -> a
showValueType val = case val of
  Object _ -> "object"
  Array _ -> "array"
  String _ -> "string"
  Number _ -> "number"
  Bool _ -> "bool"
  Null -> "null"
