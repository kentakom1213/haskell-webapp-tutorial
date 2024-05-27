{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Database.Persist.TH (derivePersistField)
import Servant (FromHttpApiData, parseQueryParam)
import Data.Text (unpack)
import Text.Read (readMaybe)

data AccountType = AccountTypeUser | AccountTypeAdmin
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "AccountType"
deriveToJSON defaultOptions ''AccountType
deriveFromJSON defaultOptions ''AccountType
instance FromHttpApiData AccountType where
    parseQueryParam = maybe (Left "HttpApiData parse error") Right . readMaybe . unpack
