{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (deriveToJSON, deriveFromJSON)
import Database.Persist.TH (derivePersistField)
import Servant (FromHttpApiData, parseQueryParam)
import Data.Text (unpack)
import Text.Read (readMaybe)

data PersonType = PersonTypeUser | PersonTypeAdmin
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
derivePersistField "PersonType"
deriveToJSON defaultOptions ''PersonType
deriveFromJSON defaultOptions ''PersonType
instance FromHttpApiData PersonType where
    parseQueryParam = maybe (Left "HttpApiData parse error") Right . readMaybe . unpack
