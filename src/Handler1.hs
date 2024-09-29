{-# LANGUAGE OverloadedStrings #-}

module Handler1 where

import ApiTypes
import Control.Exception.Safe (throwM)
import Control.Monad.Reader (asks)
import Data.Text as T (Text)
import Database.Esqueleto
import Model
import Servant
import Types
import Utils

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
  p <- fromJustWithError (err404, "No such account ID") =<< get pid

  return $ toApiAccount pid p

postAccount :: ApiAccountReqBody -> MyAppHandler ApiAccount
postAccount
  ApiAccountReqBody
    { apiAccountReqBodyName = Just name,
      apiAccountReqBodyAge = Just age,
      apiAccountReqBodyType = Just ptype
    } = errorHandler $ runSql $ do
    getAccount'
      =<< insert
        Account
          { accountName = name,
            accountAge = age,
            accountType = ptype
          }
postAccount _ = throwM err400 {errBody = "Invalid request body"}

getAccountItems :: AccountId -> MyAppHandler [ApiItem]
getAccountItems pid = errorHandler $ runSql $ do
  ilist <- select $ from $ \i -> do
    where_ (i ^. ItemAccountId ==. val pid)
    return i

  return $ toApiItemFE <$> ilist

--- Item
getItemList :: MyAppHandler [ApiItem]
getItemList = errorHandler $ runSql $ do
  ilist <- select $ from $ \i -> do
    return i

  return $ toApiItemFE <$> ilist

getItem' :: ItemId -> SqlPersistM' ApiItem
getItem' iid = do
  i <- fromJustWithError (err404, "No such item ID") =<< get iid

  return $ toApiItemFE (Entity iid i)

getItemList' :: SqlPersistM' [ApiItem]
getItemList' = do
  ilist <- select $ from $ \i -> do
    return i

  return $ toApiItemFE <$> ilist

postItem :: ApiItemReqBody -> MyAppHandler [ApiItem]
postItem
  ApiItemReqBody
    { apiItemReqBodyTitle = m_title,
      apiItemReqBodyDescription = Just description,
      apiItemReqBodyDeadline = Just deadline,
      apiItemReqBodyAccountId = Just pid
    } = errorHandler $ runSql $ do
    -- title <- maybe (throwM err400 {errBody = "Invalid request body"}) return m_title
    title <- case m_title of
      Just t -> return t
      Nothing -> throwM err400 {errBody = "Invalid request body"}
    insert_
      Item
        { itemTitle = title,
          itemDescription = description,
          itemDeadline = deadline,
          itemAccountId = pid
        }
    getItemList'
postItem _ = throwM err400 {errBody = "Invalid request body"}

deleteItem :: ItemId -> MyAppHandler [ApiItem]
deleteItem iid = errorHandler $ runSql $ do
  delete $ from $ \i -> do
    where_ (i ^. ItemId ==. val iid)
  getItemList'

-- App text
printAppText :: MyAppHandler T.Text
printAppText = do
  app_text <- asks getApplicationText
  errorHandler $ runSql $ do
    logInfo' "API app text called"
    return app_text

-- タグの追加
postTag :: Text -> MyAppHandler [ApiTag]
postTag tag = errorHandler $ runSql $ do
  insert_
    Tag
      { tagName = tag,
        tagDeletedAt = Nothing
      }
  tlist <- select $ from $ \t -> do
    return t

  return $ toApiTagFE <$> tlist
