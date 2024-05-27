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

getAccountList :: Maybe AccountType -> MyAppHandler [ApiAccount]
getAccountList maybe_ptype = errorHandler $ runSql $ do
  plist <- select $ from $ \p -> do
    where_ $ maybe (val True) (\ptype -> p ^. AccountType ==. val ptype) maybe_ptype
    return p

  return $ toApiAccountFE <$> plist

getAccount :: AccountId -> MyAppHandler ApiAccount
getAccount pid = errorHandler $ runSql $ getAccount' pid

getAccount' :: AccountId -> SqlPersistM' ApiAccount
getAccount' pid = do
  p <- fromJustWithError (err404, "No such person ID") =<< get pid

  return $ toApiAccount pid p

postAccount :: ApiAccountReqBody -> MyAppHandler ApiAccount
postAccount ApiAccountReqBody {apiAccountReqBodyName = Just name, apiAccountReqBodyAge = Just age, apiAccountReqBodyType = Just ptype} = errorHandler $ runSql $ do
  pid <- insert Account {accountName = name, accountAge = age, accountType = ptype}
  getAccount' pid

postAccount _ = throwM err400 {errBody = "Invalid request body"}

printAppText :: MyAppHandler T.Text
printAppText = do
  app_text <- asks getApplicationText
  errorHandler $ runSql $ do
    logInfo' "API app text called"
    return app_text
