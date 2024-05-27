{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module ApiTypes where

import           Data.Aeson.Internal (JSONPathElement(Key), (<?>))
import           Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import           Data.Aeson.Types
import           Data.HashMap.Strict as HM (lookup)
import           Data.Text as T (Text)
import           Data.Time (ZonedTime)
import           Database.Esqueleto (Entity(Entity))
import           GHC.Generics (Generic)

import Types
import Model
import Utils

-----------------------------------------
-- API type defintion for API response --
-----------------------------------------

data ApiPerson = ApiPerson
  { apiPersonId :: PersonId
  , apiPersonName :: Text
  , apiPersonAge :: Maybe Int
  , apiPersonType :: PersonType
  } deriving (Generic, Show)
deriveToJSON defaultOptions {fieldLabelModifier = drop 9} ''ApiPerson

toApiPerson :: PersonId -> Person -> ApiPerson
toApiPerson pid p = ApiPerson {apiPersonId = pid, apiPersonName = personName p, apiPersonAge = personAge p, apiPersonType = personType p}

toApiPersonFE :: Entity Person -> ApiPerson
toApiPersonFE (Entity pid p) = toApiPerson pid p

-- data ApiBlogPost = ApiBlogPost
--   { apiBlogPostId :: BlogPostId
--   , apiBlogPostTitle :: Text
--   , apiBlogPostAuthorId :: PersonId
--   , apiBlogPostTimestamp :: ZonedTime
--   } deriving (Generic, Show)

-- deriveToJSON defaultOptions {fieldLabelModifier = snakeKey 11} ''ApiBlogPost

-- toApiBlogPost :: BlogPostId -> BlogPost -> ApiBlogPost
-- toApiBlogPost bpid bp = ApiBlogPost {apiBlogPostId = bpid, apiBlogPostTitle = blogPostTitle bp, apiBlogPostAuthorId = blogPostAuthorId bp, apiBlogPostTimestamp = toLocalTime . blogPostTimestamp $ bp}

----------------------------------------
-- API type defintion for API request --
----------------------------------------

data ApiPersonReqBody = ApiPersonReqBody
  { apiPersonReqBodyName :: Maybe Text
  , apiPersonReqBodyAge :: Maybe (Maybe Int)
  , apiPersonReqBodyType :: Maybe PersonType
  } deriving (Generic, Show)

instance FromJSON ApiPersonReqBody where
  parseJSON = withObject "apipersonreqbody" $ \o -> do
    apiPersonReqBodyName <- o .:? "name"
    apiPersonReqBodyAge <- o .:! "age"
    apiPersonReqBodyType <- o .:? "type"
    return ApiPersonReqBody{..}

-- data ApiBlogPostReqBody = ApiBlogPostReqBody
--   { apiBlogPostReqBodyTitle :: Maybe Text
--   , apiBlogPostReqBodyAuthorInd :: Maybe PersonId
--   , apiBlogPostReqBodyTimestamp :: Maybe ZonedTime
--   } deriving (Generic, Show)

-- deriveFromJSON defaultOptions {fieldLabelModifier = snakeKey 12} ''ApiBlogPostReqBody
