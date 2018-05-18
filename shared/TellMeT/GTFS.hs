{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module TellMeT.GTFS where

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Csv (FromNamedRecord (parseNamedRecord), (.:))
import Data.Default (Default (def))
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro (Lens', (?~), at)
import Lens.Micro.GHC ()

data Feed = Feed { _agencies :: Map Text Agency
                 , _stops :: Map Text Stop
                 , _routes :: Map Text Route
                 } deriving (Eq, Show)

instance Default Feed where
  def = Feed { _agencies = mempty
             , _stops = mempty
             , _routes = mempty
             }

agencies :: Lens' Feed (Map Text Agency)
agencies f feed = (\a -> feed { _agencies = a }) <$> f (_agencies feed)

stops :: Lens' Feed (Map Text Stop)
stops f feed = (\s -> feed { _stops = s }) <$> f (_stops feed)

routes :: Lens' Feed (Map Text Route)
routes f feed = (\r -> feed { _routes = r }) <$> f (_routes feed)

class Identified a where
  identifier :: a -> Text

putMap :: (Identified a) => Lens' t (Map Text a) -> a -> t -> t
putMap part item = part . at (identifier item) ?~ item

data Agency = Agency { agency_id :: Text
                     , agency_name :: Text
                     , agency_url :: Text
                     , agency_timezone :: Text
                     , agency_lang :: Maybe Text
                     , agency_phone :: Maybe Text
                     , agency_fare_url :: Maybe Text
                     , agency_email :: Maybe Text
                     } deriving (Eq, Show, Generic)

instance Identified Agency where
  identifier = agency_id

instance FromJSON Agency
instance ToJSON Agency

instance FromNamedRecord Agency where
  parseNamedRecord m = Agency <$>
    (m .: "agency_id" <|> return "") <*>
    m .: "agency_name" <*>
    m .: "agency_url" <*>
    m .: "agency_timezone" <*>
    (m .: "agency_lang" <|> return Nothing) <*>
    (m .: "agency_phone" <|> return Nothing) <*>
    (m .: "agency_fare_url" <|> return Nothing) <*>
    (m .: "agency_email" <|> return Nothing)

data LocationType = StopType | Station | Entrance
                  deriving (Eq, Enum, Ord, Show)
instance FromJSON LocationType where
  parseJSON = withText "LocationType" $ \s ->
    case s of
      "stop" -> return StopType
      "station" -> return Station
      "entrance" -> return Entrance
      _ -> fail "invalid LocationType"
instance ToJSON LocationType where
  toJSON StopType = toJSON ("stop" :: Text)
  toJSON Station = toJSON ("station" :: Text)
  toJSON Entrance = toJSON ("entrance" :: Text)

data Stop = Stop { stop_id :: Text
                 , stop_code :: Text
                 , stop_name :: Text
                 , stop_desc :: Text
                 , stop_lat :: Float
                 , stop_lon :: Float
                 , zone_id :: Text
                 , stop_url :: Text
                 , location_type :: LocationType
                 , parent_station :: Text
                 , stop_timezone :: Text
                 , wheelchair_boarding :: Maybe Bool
                 } deriving (Eq, Show, Generic)

instance Identified Stop where
  identifier = stop_id

instance FromJSON Stop
instance ToJSON Stop

instance FromNamedRecord Stop where
  parseNamedRecord m = Stop <$>
    m .: "stop_id" <*>
    (m .: "stop_code" <|> return "") <*>
    m .: "stop_name" <*>
    (m .: "stop_desc" <|> return "") <*>
    m .: "stop_lat" <*>
    m .: "stop_lon" <*>
    (m .: "zone_id" <|> return "") <*>
    (m .: "stop_url" <|> return "") <*>
    (toEnum <$> m .: "location_type" <|> return StopType) <*>
    (m .: "parent_station" <|> return "") <*>
    (m .: "stop_timezone" <|> return "") <*>
    (accessibility <$> m .: "wheelchair_boarding" <|> return Nothing)
    where accessibility :: Int -> Maybe Bool
          accessibility 1 = Just True
          accessibility 2 = Just False
          accessibility _ = Nothing

data RouteType = LightRail | Subway | Rail | Bus | Ferry | CableCar
               | Gondola | Funicular
                 deriving (Eq, Enum, Ord, Show)
instance FromJSON RouteType where
  parseJSON = withText "RouteType" $ \s ->
    case s of
      "light_rail" -> return LightRail
      "subway" -> return Subway
      "rail" -> return Rail
      "bus" -> return Bus
      "ferry" -> return Ferry
      "cable_car" -> return CableCar
      "gondola" -> return Gondola
      "funicular" -> return Funicular
      _ -> fail "invalid RouteType"
instance ToJSON RouteType where
  toJSON LightRail = toJSON ("light_rail" :: Text)
  toJSON Subway = toJSON ("subway" :: Text)
  toJSON Rail = toJSON ("rail" :: Text)
  toJSON Bus = toJSON ("bus" :: Text)
  toJSON Ferry = toJSON ("ferry" :: Text)
  toJSON CableCar = toJSON ("cable_car" :: Text)
  toJSON Gondola = toJSON ("gondola" :: Text)
  toJSON Funicular = toJSON ("funicular" :: Text)

data Route = Route { route_id :: Text
                   , route_agency_id :: Text
                   , route_short_name :: Text
                   , route_long_name :: Text
                   , route_desc :: Text
                   , route_type :: RouteType
                   , route_url :: Text
                   , route_color :: Text
                   , route_text_color :: Text
                   , route_sort_order :: Int
                   } deriving (Eq, Show, Generic)

instance Identified Route where
  identifier = route_id

instance FromJSON Route
instance ToJSON Route

instance FromNamedRecord Route where
  parseNamedRecord m = Route <$>
    m .: "route_id" <*>
    (m .: "agency_id" <|> return "") <*>
    m .: "route_short_name" <*>
    m .: "route_long_name" <*>
    (m .: "route_desc" <|> return "") <*>
    (toEnum <$> m .: "route_type") <*>
    (m .: "route_url" <|> return "") <*>
    (m .: "route_color" <|> return "FFFFFF") <*>
    (m .: "route_text_color" <|> return "000000") <*>
    (m .: "route_sort_order" <|> return 0)
