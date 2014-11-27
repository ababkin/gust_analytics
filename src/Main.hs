{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Main where

import qualified Data.Vector                          as V
import           Database.PostgreSQL.Simple           (connect, connectDatabase,
                                                       connectUser,
                                                       defaultConnectInfo)
import qualified Database.PostgreSQL.Simple           as PGS
{- import           Database.PostgreSQL.Simple.HStore (HStoreMap (..)) -}
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)
{- import Control.Monad -}
import           Control.Applicative                  ((<$>), (<*>))
import           Data.Aeson                           (Result (..),
                                                       eitherDecode, fromJSON)
import           Data.Aeson.Types                     (FromJSON (..), Value)
import qualified Data.ByteString.Lazy.Char8           as BL8
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.String                          (fromString)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
{- import           Data.Time.Clock                   (UTCTime) -}
import           Data.Maybe                           (fromMaybe)
import           Data.Monoid
import           Data.Time.LocalTime                  (LocalTime)
import           GHC.Generics


data Config = Config {
    dbname   :: String
  , username :: String
  , query    :: String
} deriving (Show, Generic)
instance FromJSON Config


{- type LogEntry = (Maybe Text, Maybe Text, LocalTime, Value) -}

type Props = Map Text Text
data LogEntry = LogEntry {
      eventName  :: Maybe Text
    , trackingId :: Maybe Text
    , createdAt  :: LocalTime
    , props      :: Props
  }

instance FromRow LogEntry where
  fromRow = LogEntry <$>
                  field
              <*> field
              <*> field
              <*> (( M.map (\(Just v) -> v) . M.filter (/= Nothing) ) <$> field)

instance FromField (Map Text (Maybe Text)) where
  fromField = fromJSONField

data Stats = Stats{
    userTotals :: Map Text Int
  } deriving Show

initStats = Stats M.empty

main = do
  configJson <- BL8.readFile "config.json"
  case (eitherDecode configJson) of
    Left err ->
      putStrLn $ "failed to parse config.json file: " ++ (show err)
    Right config -> do
      conn <- connect defaultConnectInfo {
          connectDatabase = dbname config
        , connectUser     = username config
      }

      total <- PGS.fold_ conn (fromString $ query config) initStats consumer
      putStr $ "totals: " ++ (show total)

  where
    consumer :: Stats -> LogEntry -> IO Stats
    consumer stats le =
      case M.lookup "actor_id" (props le) of
        Nothing       -> return stats
        Just actorId  -> return $ stats{userTotals = M.alter ( Just . (+1) . fromMaybe 0) actorId (userTotals stats)}

