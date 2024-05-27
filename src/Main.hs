{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad                        (unless)
import           Control.Monad.Logger                 (logInfoNS, runNoLoggingT,
                                                       runStdoutLoggingT)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource         (runResourceT)
import           Data.Text                            as T (Text, pack)
import           Database.Persist.MySQL               (ConnectionPool,
                                                       createMySQLPool,
                                                       runMigration, runSqlPool)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (Destination (Callback),
                                                       OutputFormat (Detailed),
                                                       RequestLoggerSettings (..),
                                                       logStdout,
                                                       mkRequestLogger)
import           Options.Applicative
import           Servant                              as Sv
import           System.Log.FastLogger                (fromLogStr)

import           ApiTypes
import           DBSetting
import           Model
import           Types
import           Utils

import           Handler1

------------------------
-- Running API Server --
------------------------

main :: IO ()
main = do
  -- command line parser
  let opts_info = info (helper <*> commandLineParser) (fullDesc <> progDesc "'Servant + Persist' reference program" <> header "Description header")
  opts <- execParser opts_info

  -- application port setting
  let port = commandLineOptionsPort opts

  -- logger setting
  (m_loggingT, logger) <- case commandLineOptionsLogging opts of
    LoggingSyslog -> do
      -- logSyslog <- mkRequestLogger def {destination = Callback $ runSyslogLoggingT . logInfoNS "Log" . decodeUtf8 . fromLogStr, outputFormat = Detailed False}
      -- return (Just runSyslogLoggingT, logSyslog)
      return (Nothing, id)
    LoggingStdout -> return (Just runStdoutLoggingT, logStdout)
    LoggingNone -> return (Nothing, id)

  -- DB connection setting
  let connect_info = case commandLineOptionsDBConnection opts of
        DBConn1 -> connectInfo1
        DBConn2 -> connectInfo2

  -- pool size setting and make pool
  let pool_size = commandLineOptionsPoolSize opts
  pool <- case m_loggingT of
    Just loggingT -> loggingT $ createMySQLPool connect_info pool_size
    Nothing       -> runNoLoggingT $ createMySQLPool connect_info pool_size

  -- 'readerT Config' setting
  let cfg = MyAppConfig {getPool = pool, getApplicationText = T.pack . commandLineOptionsApplicationText $ opts, getApplicationFlag = commandLineOptionsApplicationFlag opts}

  -- Do migration
  unless (commandLineOptionsNoMigration opts) $ doMigration pool

  -- Run Application with Config
  print $ "running port: " ++ show port
  print $ "logger " ++ show (commandLineOptionsLogging opts)
  run port $ logger $ myAppApp cfg

doMigration :: ConnectionPool -> IO ()
doMigration pool = runResourceT $ runSqlPool (runMigration migrateAll) pool

----------------------------
-- API Server application --
----------------------------

myAppApi :: Proxy MyAppAPI
myAppApi = Proxy

myAppApp :: MyAppConfig -> Application
myAppApp cfg = serve myAppApi $ hoistServer myAppApi (nt cfg) myAppServer

nt :: MyAppConfig -> MyAppHandler a -> Handler a
nt s x = runReaderT x s

------------------------------
-- API Handler registration --
------------------------------

type MyAppAPI' = "person" :> Capture "person_id" AccountId :> Get '[JSON] ApiPerson
            :<|> "person" :> ReqBody '[JSON] ApiPersonReqBody :> Post '[JSON] ApiPerson
            :<|> "persons" :> QueryParam "type" PersonType :> Get '[JSON] [ApiPerson]
            :<|> "app_text" :> Get '[JSON] T.Text

-- add "/api" prefix
type MyAppAPI = "api" :> MyAppAPI'

myAppServer :: ServerT MyAppAPI MyAppHandler
myAppServer = getPerson
         :<|> postPerson
         :<|> getPersonList
         :<|> printAppText

-----------------------------
-- Command line processing --
-----------------------------

data LoggingOption = LoggingSyslog | LoggingStdout | LoggingNone
  deriving (Read, Show)

data DBConnectionOption = DBConn1 | DBConn2
  deriving (Read, Show)

data CommandLineOptions = CommandLineOptions
  { commandLineOptionsPort            :: Int
  , commandLineOptionsPoolSize        :: Int
  , commandLineOptionsDBConnection    :: DBConnectionOption
  , commandLineOptionsLogging         :: LoggingOption
  , commandLineOptionsNoMigration     :: Bool
  , commandLineOptionsApplicationText :: String
  , commandLineOptionsApplicationFlag :: Bool
  }

commandLineParser :: Parser CommandLineOptions
commandLineParser = CommandLineOptions
     <$> option auto
         ( long "port" <> short 'p'
        <> help "Server port number"
        <> showDefault
        <> value 3000
        <> metavar "<Num>" )
     <*> option auto
         ( long "pool_size" <> short 'u'
        <> help "Connection pool size for DB connection"
        <> showDefault
        <> value 8
        <> metavar "<Num>" )
     <*> option auto
         ( long "dbconn" <> short 'd'
        <> help "DB connection target"
        <> showDefault
        <> value DBConn1
        <> metavar "DBConn1/DBConn2" )
     <*> option auto
         ( long "logging" <> short 'l'
        <> help "Logging options"
        <> showDefault
        <> value LoggingSyslog
        <> metavar "LoggingSyslog/LoggingStdout/LoggingNone)" )
     <*> switch
         ( long "no_migration" <> short 'm'
        <> help "Don't do migration" )
     <*> strOption
         ( long "application_text" <> short 'a'
        <> metavar "String"
        <> value "Default application text"
        <> help "Application string used inside handler" )
     <*> switch
         ( long "appliaction_flag" <> short 'A'
        <> help "Application flag used inside handler" )
