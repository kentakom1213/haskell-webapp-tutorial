{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Model where

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Database.Persist.TH

import           Types


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

    deriving Show
|]

  -- Item
  --   name Text
  --   accountId AccountId

  --   deriving Show


-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
--   Person
--     name Text
--     age Int Maybe
--     type PersonType

--     deriving Show

--   BlogPost
--     title Text
--     authorId PersonId
--     timestamp UTCTime

--     deriving Show

--   Follow
--     follower PersonId
--     followed PersonId

--     deriving Show
-- |]
