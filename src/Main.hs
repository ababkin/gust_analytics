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

type Props = Map Text (Maybe Text)
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
              <*> field

instance FromField Props where
  fromField = fromJSONField

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

      total <- PGS.fold_ conn (fromString $ query config) 0 consumer
      putStr $ "total: " ++ (show total)

  where
    {- consumer :: (Show a, Monoid a) => a -> LogEntry -> IO a -}
    consumer :: Int -> LogEntry -> IO Int
    {- consumer :: [String] -> LogEntry -> IO [String] -}
    consumer c le =
      {- return $ c + 1 -}
      {- return $ c <> [show $ props le] -}
      if
        case M.lookup "actor_id" (props le) of
          Nothing       -> False
          Just (Just "345933") -> True
          Just _        -> False

        then return $ c + 1
        else return c
