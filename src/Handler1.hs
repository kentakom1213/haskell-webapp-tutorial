{-# LANGUAGE OverloadedStrings #-}

module Handler1 where

import ApiTypes
import Control.Exception.Safe (throwM)
import Control.Monad.Reader (asks)
import Data.Aeson.Encoding (list)
import qualified Data.Aeson.KeyMap as List
import qualified Data.Aeson.KeyMap as Map
import Data.List as List (elem, find, nub)
import Data.Map (Map, empty, findWithDefault, fromList, fromListWith, lookup, mapMaybe, toList)
import qualified Data.Maybe as Map
import Data.Maybe as Maybe (mapMaybe, fromMaybe)
import Data.Text as T (Text)
import Database.Esqueleto
    ( Entity(Entity),
      PersistStoreRead(get),
      PersistStoreWrite(insert_, insert),
      (==.),
      (?.),
      (^.),
      delete,
      from,
      in_,
      just,
      on,
      select,
      val,
      valList,
      where_,
      LeftOuterJoin(LeftOuterJoin) )
import Database.Esqueleto (valList, where_)
import Debug.Trace
import Model
import Model (EntityField (TagId), TagItem (tagItemTagId))
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

getAccountItems :: AccountId -> MyAppHandler ApiItemList
getAccountItems pid = errorHandler $ runSql $ do
  ilist <- select $ from $ \i -> do
    where_ (i ^. ItemAccountId ==. val pid)
    return i

  return $ toApiItemFE Map.empty <$> ilist

--- Item
getItemList :: MyAppHandler ApiItemList
getItemList = errorHandler $ runSql $ do
  getItemList'

getItem' :: ItemId -> SqlPersistM' ApiItem
getItem' iid = do
  i <- fromJustWithError (err404, "No such item ID") =<< get iid

  return $ toApiItemFE Data.Map.empty (Entity iid i)

getItemList' :: SqlPersistM' ApiItemList
getItemList' = do
  -- SELECT * FROM item LEFT OUTER JOIN tag_item ON tag_item.item_id = item.id;
  list' <- select $ from $ \(i `LeftOuterJoin` ti) -> do
    on $ just (i ^. ItemId) ==. ti ?. TagItemItemId
    return (i, ti)

  let ilist = snd <$> toList (Data.Map.fromList $ (\(Entity iid i, _) -> (iid, Entity iid i)) <$> list')

      itmap = Data.Map.fromListWith (++) $ (\(Entity iid i, me_ti) -> (iid, maybe [] (\(Entity _ ti) -> [tagItemTagId ti]) me_ti)) <$> list'

      tilist = Maybe.mapMaybe snd list'

      tagidlist = nub $ (\(Entity _ ti) -> tagItemTagId ti) <$> tilist

  -- traceM $ show ilist
  -- traceM $ show itmap

  -- tag テーブルから該当する tag を取得
  taglist <- select $ from $ \t -> do
    where_ $ t ^. TagId `in_` valList tagidlist
    return t

  let apiitem = toApiItemFE itmap <$> ilist
      apitag = toApiTagFE <$> taglist

  return $ ApiItemList {apiItemListItem = apiitem, apiItemListTag = apitag}

postItem :: ApiItemReqBody -> MyAppHandler ApiItemList
postItem
  ApiItemReqBody
    { apiItemReqBodyTitle = m_title,
      apiItemReqBodyDescription = Just description,
      apiItemReqBodyDeadline = Just deadline,
      apiItemReqBodyAccountId = Just pid,
      apiItemReqBodyParentId = parent_id
    } = errorHandler $ runSql $ do
    title <- case m_title of
      Just t -> return t
      Nothing -> throwM err400 {errBody = "Invalid request body"}

    -- parent_id を持つ item が存在するか確認
    -- → 存在する場合 → Just parent_id
    -- → 存在しない場合 → エラー
    -- → Nothing の場合は、そのまま
    valid_parent_id <- case parent_id of
      Just pid' -> do
        _ <- fromJustWithError (err400, "No such parent item ID") =<< get pid'
        return $ Just pid'
      Nothing -> return Nothing

    insert_
      Item
        { itemTitle = title,
          itemDescription = description,
          itemDeadline = deadline,
          itemAccountId = pid,
          itemParentId = valid_parent_id
        }
    getItemList'
postItem ng = trace ("Invalid Value: " ++ show ng) throwM err400 {errBody = "Invalid request body"}

deleteItem :: ItemId -> MyAppHandler ApiItemList
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
  getTagList'

-- タグ一覧の取得
getTagList :: MyAppHandler [ApiTag]
getTagList = errorHandler $ runSql $ do
  getTagList'

getTagList' :: SqlPersistM' [ApiTag]
getTagList' = do
  tlist <- select $ from $ \t -> do
    return t

  return $ toApiTagFE <$> tlist
