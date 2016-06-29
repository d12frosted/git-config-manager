{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main (main) where

--------------------------------------------------------------------------------
-- * Internal imports

--------------------------------------------------------------------------------
-- * External imports

import           Control.Exception
import           Control.Monad.Catch    (MonadThrow (..))
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.ByteString.Lazy   (readFile)
import           Data.HashMap.Strict    as Map
import           Data.String            (IsString (..))
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text.IO           as Txt (putStr, putStrLn)
import           Data.Typeable
import           Options.Applicative
import           Path.Parse
import           Prelude                hiding (print, putStr, putStrLn,
                                         readFile)
import qualified Turtle

--------------------------------------------------------------------------------
-- * Data types

--------------------------------------------------------------------------------
-- ** Application

data AppOptions = AppOptions { optVerbose :: Bool
                             , optFile    :: Maybe String
                             , optCommand :: AppCmd }
                 deriving Show

data AppCmd = AppCmdList | AppCmdGet | AppCmdSet String deriving Show

type App a = AppConfig -> GitConfig -> IO a

data AppConfig = AppConfig { appVerbose    :: Bool
                           , appConfigFile :: Path Abs File } deriving Show

--------------------------------------------------------------------------------
-- ** Git configuration

type ConfigMap = (Map.HashMap Text Object)
type SchemeMap = Map.HashMap Text ConfigMap
data GitConfig = GitConfig SchemeMap deriving Show

$(deriveJSON defaultOptions ''GitConfig)

--------------------------------------------------------------------------------
-- ** Exceptions

data GCMException =
  GCMParseError (Path Abs File) String |
  GCMSchemeNotFound (Path Abs File) String |
  GCMConfigTypeNotSupported String String Value
  deriving (Typeable)

instance Exception GCMException

instance Show GCMException where
  show (GCMParseError path msg) =
    "Could not parse git configurations from '" ++ toFilePath path ++ "', with error: " ++ msg
  show (GCMSchemeNotFound path scheme) =
    "Could not find scheme '" ++ scheme ++ "' in '" ++ toFilePath path ++ "'"
  show (GCMConfigTypeNotSupported section key val) =
    "Found value of unsupported type '" ++ showValueType val ++ "' at '" ++ section ++ "." ++ key ++ "'"

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
runCmd AppCmdList = listConfgs
runCmd AppCmdGet = getCurrentScheme
runCmd (AppCmdSet scheme) = setConfigs (pack scheme)

--------------------------------------------------------------------------------
-- ** List configurations

listConfgs :: App ()
listConfgs _ (GitConfig cfg) = mapM_ Txt.putStrLn $ keys cfg

--------------------------------------------------------------------------------
-- ** Get current scheme

getCurrentScheme :: App ()
getCurrentScheme _ _ = getGitConfig "gcm" "scheme" >>= Txt.putStr

--------------------------------------------------------------------------------
-- ** Set configurations

setConfigs :: Text -> App ()
setConfigs scheme (AppConfig _ path) (GitConfig cfg) =
  case Map.lookup scheme cfg of
    Nothing -> throwM $ GCMSchemeNotFound path (unpack scheme)
    Just cfgs ->
      do _ <- traverseWithKey (traverseWithKey . setGitConfig) cfgs
         setGitConfig "gcm" "scheme" (String scheme)

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

--------------------------------------------------------------------------------
-- * Git configurations

setGitConfig :: (MonadThrow m, MonadIO m) => Text -> Text -> Value -> m ()
setGitConfig section key val =
  case extractConfig val of
    Just cfg -> Turtle.procs "git" ["config", section <> "." <> key, cfg] Turtle.empty
    Nothing  -> throwM $ GCMConfigTypeNotSupported (unpack section) (unpack key) val

getGitConfig :: (MonadIO m) => Text -> Text -> m Text
getGitConfig section key = snd <$> Turtle.procStrict "git" ["config", section <> "." <> key] Turtle.empty

--------------------------------------------------------------------------------
-- * Helpers

loadGitConfig :: (MonadThrow m, MonadIO m) => Path Abs File -> m GitConfig
loadGitConfig path =
  do contents <- liftIO . readFile . toFilePath $ path
     case eitherDecode' contents of
       Left msg -> throwM $ GCMParseError path msg
       Right cfg -> return cfg

getGitConfigPath :: (MonadThrow m, MonadIO m) => Maybe String -> m (Path Abs File)
getGitConfigPath fileStrM =
  case fileStrM of
    Just fileStr -> parseFilePath . pack $ fileStr
    Nothing -> getDefaultGitConfigPath

getDefaultGitConfigPath :: (MonadThrow m, MonadIO m) => m (Path Abs File)
getDefaultGitConfigPath = parseFilePath "$XDG_CONFIG_HOME/git/git-config-manager.json"

extractConfig :: Value -> Maybe Text
extractConfig (String val) = Just val
extractConfig (Number val) = Just . pack . show $ val
extractConfig (Bool val) = Just . prettyBool $ val
extractConfig Null = Just "null"
extractConfig _ = Nothing

prettyBool :: IsString a => Bool -> a
prettyBool True = "true"
prettyBool False = "false"

showValueType :: IsString a => Value -> a
showValueType val = case val of
  Object _ -> "object"
  Array _ -> "array"
  String _ -> "string"
  Number _ -> "number"
  Bool _ -> "bool"
  Null -> "null"
