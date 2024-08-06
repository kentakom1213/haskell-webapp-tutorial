{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module DBSetting where

import           Database.Persist.MySQL (ConnectInfo(..))
import           Database.MySQL.Base.Types (Option(InitCommand))
import           Data.Yaml (decodeFileEither, (.:), withObject)
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON, parseJSON)
import           Control.Exception (throwIO)

data DatabaseConfig = DatabaseConfig
  { host     :: String
  , port     :: Int
  , user     :: String
  , password :: String
  , database :: String
  } deriving (Show, Generic)
    deriving anyclass (FromJSON)

data DBConfig = DBConfig
  { db_conf_1 :: DatabaseConfig
  , db_conf_2 :: DatabaseConfig
  } deriving (Show, Generic)

instance FromJSON DBConfig where
  parseJSON = withObject "database" $ \v -> DBConfig
    <$> v .: "db_conf_1"
    <*> v .: "db_conf_2"

connectOptions' :: [Option]
connectOptions' = [InitCommand "SET SESSION sql_mode='TRADITIONAL,NO_AUTO_VALUE_ON_ZERO,ONLY_FULL_GROUP_BY';"
                  , InitCommand "SET NAMES utf8;"]

loadConfig :: FilePath -> IO DBConfig
loadConfig filePath = do
  result <- decodeFileEither filePath
  case result of
    Left err -> throwIO (userError (show err))
    Right config -> return config

toConnectInfo :: DatabaseConfig -> ConnectInfo
toConnectInfo dbConfig = ConnectInfo
  { connectHost     = host dbConfig
  , connectPort     = fromIntegral (port dbConfig)
  , connectUser     = user dbConfig
  , connectPassword = password dbConfig
  , connectDatabase = database dbConfig
  , connectOptions  = connectOptions'
  , connectPath     = ""
  , connectSSL      = Nothing
  }
