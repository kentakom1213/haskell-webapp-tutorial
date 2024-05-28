{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module ApiTypes where

import           Data.Aeson.Internal (JSONPathElement(Key), (<?>))
import           Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import           Data.Aeson as Json
import           Data.HashMap.Strict as HM (lookup)
import           Data.Text as T (Text)
import           Data.Time (ZonedTime)
import           Database.Esqueleto (Entity(Entity))
import           GHC.Generics (Generic)
import           Data.Proxy (Proxy (..))
import           Elm.Module (DefineElm (..), defaultTypeAlterations, makeElmModule, makeModuleContentWithAlterations, recAlterType)
import           Elm.TyRep (ETCon (..), EType (..), ETypeDef)
import           Elm.Derive

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
deriveToJSON Json.defaultOptions {fieldLabelModifier = drop 10} ''ApiAccount
deriveElmDef Json.defaultOptions {fieldLabelModifier = drop 10} ''ApiAccount

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
deriveToJSON Json.defaultOptions {fieldLabelModifier = drop 7} ''ApiItem

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


-------------------------
-- API export for Elm  --
-------------------------

myAlteration :: ETypeDef -> ETypeDef
myAlteration =
  recAlterType $ \t -> case t of
    ETyApp (ETyCon (ETCon "Key")) _ ->
      ETyCon (ETCon "Int")
    ETyCon (ETCon "Day") ->
      ETyCon (ETCon "Date")
    ETyCon (ETCon "Int64") ->
      ETyCon (ETCon "Int")
    _ ->
      defaultTypeAlterations t


elmApiExport :: String
elmApiExport =
  makeElmModule "ApiTypes" [] ++
  "import Time exposing(Posix)" ++ "\n" ++
  "import Date exposing(Date)" ++ "\n" ++
  "import MyApiDecoder exposing(..)" ++ "\n" ++
  "\n\n" ++
  makeModuleContentWithAlterations myAlteration
  [          -- [type] data types
              -- DefineElm    (Proxy :: Proxy AccountType)
            -- , DefineElm    (Proxy :: Proxy SectionType)
            -- , DefineElm    (Proxy :: Proxy LoggerStatus)
            -- , DefineElm    (Proxy :: Proxy ParameterType)
            -- , DefineElm    (Proxy :: Proxy EventType)
            -- , DefineElm    (Proxy :: Proxy AverageCalcMode)
            -- , DefineElm    (Proxy :: Proxy BatteryUpdateType)
            -- , DefineElm    (Proxy :: Proxy TaskStatus)
            -- , DefineElm    (Proxy :: Proxy ExcelSheet)
            -- , DefineElm    (Proxy :: Proxy NotificationType)
            -- , DefineElm    (Proxy :: Proxy NotificationExecType)
            -- , DefineElm    (Proxy :: Proxy MaintenanceType)
            -- , DefineElm    (Proxy :: Proxy LeakageType)
            -- , DefineElm    (Proxy :: Proxy LeakageDetectionType)
            -- , DefineElm    (Proxy :: Proxy LeakageRoadType)
            -- , DefineElm    (Proxy :: Proxy LeakageSurfaceType)
            -- , DefineElm    (Proxy :: Proxy LeakageSituationType)
            -- , DefineElm    (Proxy :: Proxy FileType)
            -- , DefineElm    (Proxy :: Proxy JudgementMethod)
            -- , DefineElm    (Proxy :: Proxy ServicePlanType)

            -- [type] ID
            DefineElm    (Proxy :: Proxy AccountId)
            , DefineElm    (Proxy :: Proxy ApiAccount)
            -- , DefineElm    (Proxy :: Proxy LoggerId)
            -- , DefineElm    (Proxy :: Proxy LoggerHistoryId)
            -- , DefineElm    (Proxy :: Proxy SectionId)
            -- , DefineElm    (Proxy :: Proxy ProjectId)
            -- , DefineElm    (Proxy :: Proxy LocationId)
            -- , DefineElm    (Proxy :: Proxy Catm1ReportDataId)
            -- , DefineElm    (Proxy :: Proxy Catm1SensorDataId)
            -- , DefineElm    (Proxy :: Proxy Catm1ReportResultId)
            -- , DefineElm    (Proxy :: Proxy DevParameterId)
            -- , DefineElm    (Proxy :: Proxy AppParameterId)
            -- , DefineElm    (Proxy :: Proxy JdgParameterId)
            -- , DefineElm    (Proxy :: Proxy MtmParameterId)
            -- , DefineElm    (Proxy :: Proxy AttachmentId)
            -- , DefineElm    (Proxy :: Proxy MaintenanceId)
            -- , DefineElm    (Proxy :: Proxy AnnounceId)
            -- , DefineElm    (Proxy :: Proxy LineGroupId)
            -- , DefineElm    (Proxy :: Proxy LineEventId)
            -- , DefineElm    (Proxy :: Proxy CommModuleId)
            -- , DefineElm    (Proxy :: Proxy CommModuleBatteryId)

            -- , DefineElm    (Proxy :: Proxy LncDecisionCommand)
            -- , DefineElm    (Proxy :: Proxy LncDecisionSerialNo)
            -- , DefineElm    (Proxy :: Proxy LncDecisionTimestamp)
  ]
