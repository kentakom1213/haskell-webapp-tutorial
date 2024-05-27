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

data ApiAccount = ApiAccount
  { apiAccountId :: AccountId
  , apiAccountName :: Text
  , apiAccountAge :: Maybe Int
  , apiAccountType :: AccountType
  } deriving (Generic, Show)
deriveToJSON defaultOptions {fieldLabelModifier = drop 9} ''ApiAccount

toApiAccount :: AccountId -> Account -> ApiAccount
toApiAccount pid p = ApiAccount {apiAccountId = pid, apiAccountName = accountName p, apiAccountAge = accountAge p, apiAccountType = accountType p}

toApiAccountFE :: Entity Account -> ApiAccount
toApiAccountFE (Entity pid p) = toApiAccount pid p

data ApiItem = ApiItem
  { apiItemId :: ItemId
  , apiItemTitle :: Text
  , apiItemDescription :: Text
  , apiItemDeadline :: ZonedTime
  , apiItemAccountId :: AccountId
  } deriving (Generic, Show)
deriveToJSON defaultOptions {fieldLabelModifier = drop 7} ''ApiItem

toApiItem :: ItemId -> Item -> ApiItem
toApiItem iid i = ApiItem
  { apiItemId = iid
  , apiItemTitle = itemTitle i
  , apiItemDescription = itemDescription i
  , apiItemDeadline = toLocalTime (itemDeadline i)
  , apiItemAccountId = itemAccountId i
  }

toApiItemFE :: Entity Item -> ApiItem
toApiItemFE (Entity iid i) = toApiItem iid i

-- data ApiBlogPost = ApiBlogPost
--   { apiBlogPostId :: BlogPostId
--   , apiBlogPostTitle :: Text
--   , apiBlogPostAuthorId :: AccountId
--   , apiBlogPostTimestamp :: ZonedTime
--   } deriving (Generic, Show)

-- deriveToJSON defaultOptions {fieldLabelModifier = snakeKey 11} ''ApiBlogPost

-- toApiBlogPost :: BlogPostId -> BlogPost -> ApiBlogPost
-- toApiBlogPost bpid bp = ApiBlogPost {apiBlogPostId = bpid, apiBlogPostTitle = blogPostTitle bp, apiBlogPostAuthorId = blogPostAuthorId bp, apiBlogPostTimestamp = toLocalTime . blogPostTimestamp $ bp}

----------------------------------------
-- API type defintion for API request --
----------------------------------------

data ApiAccountReqBody = ApiAccountReqBody
  { apiAccountReqBodyName :: Maybe Text
  , apiAccountReqBodyAge :: Maybe (Maybe Int)
  , apiAccountReqBodyType :: Maybe AccountType
  } deriving (Generic, Show)

instance FromJSON ApiAccountReqBody where
  parseJSON = withObject "apipersonreqbody" $ \o -> do
    apiAccountReqBodyName <- o .:? "name"
    apiAccountReqBodyAge <- o .:! "age"
    apiAccountReqBodyType <- o .:? "type"
    return ApiAccountReqBody{..}

data ApiItemReqBody = ApiItemReqBody
  { apiItemReqBodyTitle :: Maybe Text
  , apiItemReqBodyDescription :: Maybe Text
  , apiItemReqBodyDeadline :: Maybe ZonedTime
  , apiItemReqBodyAccountId :: Maybe AccountId
  } deriving (Generic, Show)

instance FromJSON ApiItemReqBody where
  parseJSON = withObject
    "apiitemreqbody"
    (\o -> do
       apiItemReqBodyTitle <- o .:? "title"
       apiItemReqBodyDescription <- o .:? "description"
       apiItemReqBodyDeadline <- o .:? "deadline"
       apiItemReqBodyAccountId <- o .:? "account_id"
       return ApiItemReqBody {..})

-- data ApiBlogPostReqBody = ApiBlogPostReqBody
--   { apiBlogPostReqBodyTitle :: Maybe Text
--   , apiBlogPostReqBodyAuthorInd :: Maybe AccountId
--   , apiBlogPostReqBodyTimestamp :: Maybe ZonedTime
--   } deriving (Generic, Show)

-- deriveFromJSON defaultOptions {fieldLabelModifier = snakeKey 12} ''ApiBlogPostReqBody
