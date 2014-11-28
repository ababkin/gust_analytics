{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import qualified Data.Vector                as V
import           Database.PostgreSQL.Simple (connect, connectDatabase,
                                             connectUser, defaultConnectInfo)
import qualified Database.PostgreSQL.Simple as PGS
{- import           Database.PostgreSQL.Simple.HStore (HStoreMap (..)) -}

import           Control.Applicative        ((<$>), (<*>))
import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Config                     (Config (..), withConfig)
import           Types.Event                (Actor (..), Event (..),
                                             Subject (..), actor, aid)


data Stats = Stats{
    _typeTotals  :: Map Text Int
  , _actorTotals :: Map Int Int
  } deriving Show
makeLenses ''Stats

initStats = Stats M.empty M.empty

main :: IO ()
main = do
  withConfig $ \config -> do
    conn <- connect defaultConnectInfo {
        connectDatabase = dbname config
      , connectUser     = username config
    }

    total <- PGS.fold_ conn (fromString $ query config) initStats logConsumer
    putStr $ "totals: " ++ (show total)

  where
    logConsumer :: Stats -> Event -> IO Stats
    logConsumer stats e = fmap snd $ (flip runStateT) stats $ do
      trackActor e
      case e of
        Search {}       -> do
          trackType "search"
        UnknownEvent {} -> do
          trackType "unknown"

      where
        trackType et = typeTotals %= M.alter incr et
        trackActor a = actorTotals %= M.alter incr (a ^. actor . aid)

        incr = Just . (+1) . fromMaybe 0
