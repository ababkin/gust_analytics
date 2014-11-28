{-# LANGUAGE DeriveGeneric #-}

module Config where

import           Data.Aeson                 (eitherDecode)
import           Data.Aeson.Types           (FromJSON (..))
import qualified Data.ByteString.Lazy.Char8 as BL8
import           GHC.Generics


data Config = Config {
    dbname   :: String
  , username :: String
  , query    :: String
} deriving (Show, Generic)
instance FromJSON Config

withConfig :: (Config -> IO ()) -> IO ()
withConfig cb = do
  configJson <- BL8.readFile "config.json"
  either
    (putStrLn . ("failed to parse config.json file: " ++) . show)
    cb
    $ eitherDecode configJson

