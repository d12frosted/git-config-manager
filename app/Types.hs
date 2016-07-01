{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Types (AppConfig (..), GCMException(..), Value(..)) where

--------------------------------------------------------------------------------
-- * Internal imports

--------------------------------------------------------------------------------
-- * External imports

import           Control.Exception
import           Data.Aeson
import           Data.String       (IsString (..))
import           Data.Typeable
import           Path.Parse
import           Prelude           hiding (print, putStr, putStrLn, readFile)

--------------------------------------------------------------------------------
-- * Data types

data AppConfig = AppConfig { appVerbose    :: Bool
                           , appConfigFile :: Path Abs File } deriving Show

--------------------------------------------------------------------------------
-- ** Exceptions

data GCMException =
  GCMParseError (Path Abs File) String |
  GCMSchemeNotFound (Path Abs File) String |
  GCMConfigTypeNotSupported String String Value |
  GCMSchemesParseError String
  deriving (Typeable)

instance Exception GCMException

instance Show GCMException where
  show (GCMParseError path msg) =
    "Could not parse git configurations from '" ++ toFilePath path ++ "', with error: " ++ msg
  show (GCMSchemeNotFound path scheme) =
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
