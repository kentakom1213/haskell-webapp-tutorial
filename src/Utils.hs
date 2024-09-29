{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Cases (snakify)
import Control.Exception (ErrorCall)
import Control.Exception.Safe as Ex (Handler (Handler), catches, throwM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (logDebugNS, logErrorNS, logInfoNS, logWarnNS, runLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, pack)
import Data.Char (toLower)
import Data.Text as T (Text, pack, unpack)
import Data.Time (TimeZone (TimeZone), UTCTime, ZonedTime, addUTCTime, getCurrentTime, utcToZonedTime, zonedTimeToUTC)
import Database.MySQL.Base (MySQLError)
import Database.Persist.MySQL (ConnectionPool, PersistException, SqlPersistT, runSqlPool)
import Database.Persist.Sql.Types.Internal (connLogFunc)
import Servant as Sv

---------------------
-- Type definition --
---------------------

data MyAppConfig = MyAppConfig
  { getPool :: ConnectionPool,
    -- sample 'global settings'
    getApplicationText :: T.Text,
    getApplicationFlag :: Bool
  }

type MyAppHandler = ReaderT MyAppConfig Sv.Handler

type SqlPersistM' = SqlPersistT (ResourceT (ReaderT MyAppConfig IO))

----------------------------
-- SQL and error handlers --
----------------------------

runSql :: (MonadReader MyAppConfig m, MonadIO m) => SqlPersistM' b -> m b
runSql query = do
  config <- ask
  pool <- asks getPool
  liftIO $ flip runReaderT config $ runResourceT $ runSqlPool query pool

errorHandler :: MyAppHandler a -> MyAppHandler a
errorHandler =
  flip
    catches
    [ Ex.Handler
        ( \(e :: ServerError) -> do
            runSql $ logError' $ T.pack $ show e
            throwError e
        ),
      Ex.Handler
        ( \(e :: PersistException) -> do
            runSql $ logError' $ T.pack $ show e
            throwError err500 {errBody = LBS.pack $ show e}
        ),
      Ex.Handler
        ( \(e :: MySQLError) -> do
            runSql $ logError' $ T.pack $ show e
            throwError err400 {errBody = LBS.pack $ show e}
        ),
      Ex.Handler
        ( \(e :: ErrorCall) -> do
            runSql $ logError' $ T.pack $ show e
            throwError err400 {errBody = LBS.pack $ show e}
        )
    ]

fromJustWithError :: (ServerError, LBS.ByteString) -> Maybe a -> SqlPersistM' a
fromJustWithError (err, ebody) Nothing = throwM err {errBody = ebody}
fromJustWithError _ (Just a) = return a

headWithError :: (ServerError, LBS.ByteString) -> [a] -> SqlPersistM' a
headWithError (err, ebody) [] = throwM err {errBody = ebody}
headWithError _ a = return $ head a

rightWithError :: (ServerError, LBS.ByteString) -> Either l r -> SqlPersistM' r
rightWithError (err, ebody) (Left _) = throwM err {errBody = ebody}
rightWithError _ (Right r) = return r

------------------------------------
-- debuging functions under runDB --
------------------------------------
logDebug' :: (MonadIO m) => T.Text -> SqlPersistT m ()
logDebug' message = do
  sqlbackend <- ask
  runLoggingT (logDebugNS "Log" message) $ connLogFunc sqlbackend

logInfo' :: (MonadIO m) => T.Text -> SqlPersistT m ()
logInfo' message = do
  sqlbackend <- ask
  runLoggingT (logInfoNS "Log" message) $ connLogFunc sqlbackend

logWarn' :: (MonadIO m) => T.Text -> SqlPersistT m ()
logWarn' message = do
  sqlbackend <- ask
  runLoggingT (logWarnNS "Log" message) $ connLogFunc sqlbackend

logError' :: (MonadIO m) => T.Text -> SqlPersistT m ()
logError' message = do
  sqlbackend <- ask
  runLoggingT (logErrorNS "Log" message) $ connLogFunc sqlbackend

-------------------------
-- Timezone conversion --
-------------------------
timeZoneHours :: Int
timeZoneHours = 9

timeZone :: TimeZone
timeZone = TimeZone (60 * timeZoneHours) False "JST"

toLocalTime :: UTCTime -> ZonedTime
toLocalTime = utcToZonedTime timeZone . addUTCTime ((-3600) * fromIntegral timeZoneHours)

fromLocalTime :: ZonedTime -> UTCTime
fromLocalTime = addUTCTime (3600 * fromIntegral timeZoneHours) . zonedTimeToUTC

getLocalTime :: IO UTCTime
getLocalTime = addUTCTime (3600 * fromIntegral timeZoneHours) <$> getCurrentTime

-------------------------
-- API key generation  --
-------------------------

snakeKey :: Int -> String -> String
snakeKey drop_count = T.unpack . snakify . T.pack . drop drop_count

camelKey :: Int -> String -> String
camelKey dropcount msg = case msg_body of
  msg_body_1 : msg_body_rest -> toLower msg_body_1 : msg_body_rest
  [] -> []
  where
    msg_body = drop dropcount msg
