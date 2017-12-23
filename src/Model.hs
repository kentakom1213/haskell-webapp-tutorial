{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model where

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Database.Persist.TH

import           Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Person
    name Text
    age Int Maybe
    type PersonType

    deriving Show

  BlogPost
    title Text
    authorId PersonId
    timestamp UTCTime

    deriving Show

  Follow
    follower PersonId
    followed PersonId

    deriving Show
|]
