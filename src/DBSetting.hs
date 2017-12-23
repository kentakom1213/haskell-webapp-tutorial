{-# LANGUAGE OverloadedStrings          #-}

module DBSetting where

import           Database.Persist.MySQL (ConnectInfo(..))
import           Database.MySQL.Base.Types (Option(InitCommand))

connectOptions' :: [Option]
connectOptions' = [InitCommand "SET SESSION sql_mode='TRADITIONAL,NO_AUTO_VALUE_ON_ZERO,ONLY_FULL_GROUP_BY';"
                  , InitCommand "SET NAMES utf8;"]

connectInfo1 :: ConnectInfo
connectInfo1 = ConnectInfo {
    connectHost     = "localhost",
    connectPort     = 3306,
    connectUser     = "root",
    connectPassword = "",
    connectDatabase = "persist_test",
    connectOptions  = connectOptions',
    connectPath     = "",
    connectSSL      = Nothing
  }

connectInfo2 :: ConnectInfo
connectInfo2 = ConnectInfo {
    connectHost     = "localhost",
    connectPort     = 3306,
    connectUser     = "root",
    connectPassword = "",
    connectDatabase = "persist_test2",
    connectOptions  = connectOptions',
    connectPath     = "",
    connectSSL      = Nothing
  }
