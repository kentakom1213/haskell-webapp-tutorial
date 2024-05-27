{-# LANGUAGE OverloadedStrings #-}

module Handler1 where

import           Control.Exception.Safe (throwM)
import           Control.Monad.Reader   (asks)
import           Data.Text              as T (Text)
import           Database.Esqueleto
import           Servant

import           ApiTypes
import           Model
import           Types
import           Utils

getPersonList :: Maybe PersonType -> MyAppHandler [ApiPerson]
getPersonList maybe_ptype = errorHandler $ runSql $ do
  plist <- select $ from $ \p -> do
    where_ $ maybe (val True) (\ptype -> p ^. AccountType ==. val ptype) maybe_ptype
    return p

  return $ toApiPersonFE <$> plist

getPerson :: AccountId -> MyAppHandler ApiPerson
getPerson pid = errorHandler $ runSql $ getPerson' pid

getPerson' :: AccountId -> SqlPersistM' ApiPerson
getPerson' pid = do
  p <- fromJustWithError (err404, "No such person ID") =<< get pid

  return $ toApiPerson pid p

postPerson :: ApiPersonReqBody -> MyAppHandler ApiPerson
postPerson ApiPersonReqBody {apiPersonReqBodyName = Just name, apiPersonReqBodyAge = Just age, apiPersonReqBodyType = Just ptype} = errorHandler $ runSql $ do
  pid <- insert Account {accountName = name, accountAge = age, accountType = ptype}
  getPerson' pid

postPerson _ = throwM err400 {errBody = "Invalid request body"}

printAppText :: MyAppHandler T.Text
printAppText = do
  app_text <- asks getApplicationText
  errorHandler $ runSql $ do
    logInfo' "API app text called"
    return app_text
