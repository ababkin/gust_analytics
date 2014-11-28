{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Types.Event where

import           Control.Lens
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromJust, fromMaybe)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Time.LocalTime                  (LocalTime)
import           Database.PostgreSQL.Simple.FromField (FromField (..),
                                                       fromJSONField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow (..), field)

type EventProps = Map Text (Maybe Text)
instance FromField EventProps where
  fromField = fromJSONField



data Subject = UserSubject Int

data Actor = Actor {
    _aid                  :: Int
  , _aLocale              :: Text
  , _aSource              :: Text
  , _aLocation            :: Text
  , _aCreatedAt           :: LocalTime
  , _aIsInvestor          :: Bool
  , _aIsEntrepreneur      :: Bool
  , _aAcquisitionSource   :: Maybe Text
  , _aAccreditationStatus :: Text

}
makeLenses ''Actor


data Startup = Startup {
    _sid                :: Int
  , _sIndustry          :: Text
  , _sLocation          :: Text
  , _sHasVideo          :: Bool
  , _sAcquisitionSource :: Maybe Text

}
makeLenses ''Startup

data Event = Search {
    _subject        :: Subject
  , _createdAt      :: LocalTime
  , _actor          :: Actor

  , _searchCategory :: Text
  , _searchIndustry :: Maybe Text
  , _searchLocation :: Maybe Text

 }
 | CompanyProfileViewed {
    _subject           :: Subject
  , _createdAt         :: LocalTime
  , _actor             :: Actor

  , _startup           :: Startup

  , _companyProfileTab :: Text

 }
 | InfoRequestSent {
    _subject   :: Subject
  , _createdAt :: LocalTime
  , _actor     :: Actor

  , _startup   :: Startup

 }
 | ShareRequestAccepted {
    _subject   :: Subject
  , _createdAt :: LocalTime
  , _actor     :: Actor

  , _startup   :: Startup

 }
 | UserAcquisition {
    _subject   :: Subject
  , _createdAt :: LocalTime
  , _actor     :: Actor

 }
 | UnknownEvent {
    _subject   :: Subject
  , _createdAt :: LocalTime
  , _actor     :: Actor

  , _eventName :: Text

 }
makeLenses ''Event



instance FromRow Event where
  fromRow = do
    eventName       <- field
    eventTrackingId <- field
    eventCreatedAt  <- field
    eventProps      <- field
    return $ case eventName of
      "search" -> Search {
          _subject    = parseSubject eventTrackingId
        , _createdAt  = eventCreatedAt
        , _actor      = parseActor eventProps

        , _searchCategory = fromJustJust  $ M.lookup "search_category" eventProps
        , _searchIndustry = fromJust      $ M.lookup "search_industry" eventProps
        , _searchLocation = fromJust      $ M.lookup "search_location" eventProps

      }

      _ -> UnknownEvent {
          _subject    = parseSubject eventTrackingId
        , _createdAt  = eventCreatedAt
        , _eventName  = eventName

        , _actor      = parseActor eventProps
      }

fromJustJust = fromJust . fromJust

parseSubject :: Text -> Subject
parseSubject t = let [subjectType, sibjectId] = T.split (=='#') t in
  case subjectType of
    "user" -> UserSubject $ read $ T.unpack sibjectId

parseActor :: EventProps -> Actor
parseActor pm = Actor {
    _aid                  = read $ T.unpack $ fromJustJust $ M.lookup "actor_id" pm
  , _aLocale              =         fromJustJust $ M.lookup "actor_locale" pm
  , _aSource              =         fromJustJust $ M.lookup "actor_source" pm
  , _aLocation            =         fromJustJust $ M.lookup "actor_location" pm
  , _aCreatedAt           = read $ T.unpack $ fromJustJust $ M.lookup "actor_created_at" pm
  , _aIsInvestor          = "investor"      == (fromJustJust $ M.lookup "actor_is_investor"     pm)
  , _aIsEntrepreneur      = "entrepreneur"  == (fromJustJust $ M.lookup "actor_is_entrepreneur" pm)
  , _aAcquisitionSource   =         fromJust     $ M.lookup "actor_acquisition_source"    pm
  , _aAccreditationStatus =         fromJustJust $ M.lookup "actor_accreditation_status"  pm

  }


