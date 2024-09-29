{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Model where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH
import Elm.Derive (defaultOptions, deriveElmDef)
import Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Account
    name Text
    age Int Maybe
    type AccountType

    deriving Show

  Item
    title Text
    description Text
    deadline UTCTime
    accountId AccountId
    parentId ItemId Maybe

    deriving Show

  Tag
    name Text
    deletedAt UTCTime Maybe

    deriving Show
  
  TagItem
    tagId TagId
    itemId ItemId

    deriving Show
|]

deriveElmDef defaultOptions ''AccountId
deriveElmDef defaultOptions ''ItemId
deriveElmDef defaultOptions ''TagId
