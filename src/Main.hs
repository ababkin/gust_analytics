{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Vector                       as V
import           Database.PostgreSQL.Simple        (connect, connectDatabase,
                                                    connectUser,
                                                    defaultConnectInfo)
import qualified Database.PostgreSQL.Simple        as PGS
import           Database.PostgreSQL.Simple.HStore (HStoreMap (..))
{- import Control.Monad -}
{- import Control.Applicative -}
import           Data.Aeson
import           Data.Aeson.Types                  (Value)
import qualified Data.Map                          as M
import qualified Data.Text                         as T

main = do
  conn <- connect defaultConnectInfo {
      connectDatabase = "gust_production"
    , connectUser     = "ababkin"
  }

  {- (stuff :: [[Props]]) <- PGS.query_ conn "SELECT properties FROM analytics_logs LIMIT 1" -}
  {- putStr $ show stuff -}
  total <- PGS.fold_ conn "SELECT properties FROM analytics_logs LIMIT 5" 0 consumer
  putStr $ "total: " ++ (show total)

  where
    consumer :: Int -> [HStoreMap] -> IO Int
    consumer c [p] =
      if
        case M.lookup "actor_id" (fromHStoreMap p) of
          Nothing       -> False
          Just "409203" -> True
          Just _        -> False

        then return $ c + 1
        else return c
