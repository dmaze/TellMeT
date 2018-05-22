{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Data representation of the General Transit Feed Specification.
--
-- This module has mostly straight ports of Google's GTFS CSV file
-- formats; see <https://developers.google.com/transit/gtfs/>.  It
-- includes instances for 'FromNamedRecord' to parse the GTFS files,
-- and 'FromJSON' and 'ToJSON' to provide a REST interface.
module TellMeT.GTFS where

import Control.Applicative ((<|>))
import Data.Aeson
  ( genericParseJSON, genericToJSON, withText
  )
import Data.Aeson.Types
  ( FromJSON (parseJSON), ToJSON (toJSON), Options (fieldLabelModifier)
  , camelTo2, defaultOptions
  )
import Data.Csv (FromNamedRecord (parseNamedRecord), (.:))
import Data.Default (Default (def))
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro (Lens', (?~), at)
import Lens.Micro.GHC ()

import TellMeT.Util
  ( Identified (identifier)
  , Identifier (Identifier)
  , MapOf
  )

-- | Data extracted from an entire GTFS feed.
data Feed = Feed { _agencies :: MapOf Text Agency
                 , _stops :: MapOf Text Stop
                 , _routes :: MapOf Text Route
                 , _trips :: MapOf Text Trip
                 } deriving (Eq, Show)

instance Default Feed where
  def = Feed { _agencies = mempty
             , _stops = mempty
             , _routes = mempty
             , _trips = mempty
             }

-- | Access the map of agency ID to agency for a feed.
agencies :: Lens' Feed (MapOf Text Agency)
agencies f feed = (\a -> feed { _agencies = a }) <$> f (_agencies feed)

-- | Access the map of stop ID to stop for a feed.
stops :: Lens' Feed (MapOf Text Stop)
stops f feed = (\s -> feed { _stops = s }) <$> f (_stops feed)

-- | Access the map of route ID to route for a feed.
routes :: Lens' Feed (MapOf Text Route)
routes f feed = (\r -> feed { _routes = r }) <$> f (_routes feed)

-- | Access the map of trip ID to trip for a feed.
trips :: Lens' Feed (MapOf Text Trip)
trips f feed = (\t -> feed { _trips = t}) <$> f (_trips feed)

-- | Add an identified object to a map, indexed by its identifier.
putMap :: (Ord k, Identified k a) => Lens' t (MapOf k a) -> a -> t -> t
putMap part item = part . at (identifier item) ?~ item

jsonOptions :: Options
jsonOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Parse a numeric value used as maybe a Boolean.
--
-- This "type" appears in a couple of places, for instance indicating
-- wheelchair accessibility or whether bicycles are allowed.
maybeBool :: Int -> Maybe Bool
maybeBool 1 = Just True
maybeBool 2 = Just False
maybeBool _ = Nothing

-- | A transit agency, as described in a GTFS feed.
data Agency = Agency { agencyId :: Identifier Text Agency
                     , agencyName :: Text
                     , agencyUrl :: Text
                     , agencyTimeZone :: Text
                     , agencyLang :: Maybe Text
                     , agencyPhone :: Maybe Text
                     , agencyFareUrl :: Maybe Text
                     , agencyEmail :: Maybe Text
                     } deriving (Eq, Show, Generic)

instance Identified Text Agency where
  identifier = agencyId

instance FromJSON Agency where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Agency where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Agency where
  parseNamedRecord m = Agency <$>
    (Identifier <$> (m .: "agency_id" <|> return "")) <*>
    m .: "agency_name" <*>
    m .: "agency_url" <*>
    m .: "agency_timezone" <*>
    (m .: "agency_lang" <|> return Nothing) <*>
    (m .: "agency_phone" <|> return Nothing) <*>
    (m .: "agency_fare_url" <|> return Nothing) <*>
    (m .: "agency_email" <|> return Nothing)

-- | The type of location in a 'Stop'.
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

-- | A place where some transit vehicle stops.
data Stop = Stop { stopId :: Identifier Text Stop
                 , stopCode :: Text
                 , stopName :: Text
                 , stopDesc :: Text
                 , stopLat :: Float
                 , stopLon :: Float
                 , zoneId :: Text
                 , stopUrl :: Text
                 , locationType :: LocationType
                 , parentStation :: Text
                 , stopTimezone :: Text
                 , wheelchairBoarding :: Maybe Bool
                 } deriving (Eq, Show, Generic)

instance Identified Text Stop where
  identifier = stopId

instance FromJSON Stop where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Stop where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Stop where
  parseNamedRecord m = Stop <$>
    (Identifier <$> m .: "stop_id") <*>
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
    (maybeBool <$> m .: "wheelchair_boarding" <|> return Nothing)

-- | The kind of vehicle serving a route.
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

-- | A single transit route as described in a GTFS feed.
data Route = Route { routeId :: Identifier Text Route
                   , routeAgencyId :: Identifier Text Agency
                   , routeShortName :: Text
                   , routeLongName :: Text
                   , routeDesc :: Text
                   , routeType :: RouteType
                   , routeUrl :: Text
                   , routeColor :: Text
                   , routeTextColor :: Text
                   , routeSortOrder :: Int
                   } deriving (Eq, Show, Generic)

instance Identified Text Route where
  identifier = routeId

instance FromJSON Route where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Route where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Route where
  parseNamedRecord m = Route <$>
    (Identifier <$> m .: "route_id") <*>
    (Identifier <$> (m .: "agency_id" <|> return "")) <*>
    m .: "route_short_name" <*>
    m .: "route_long_name" <*>
    (m .: "route_desc" <|> return "") <*>
    (toEnum <$> m .: "route_type") <*>
    (m .: "route_url" <|> return "") <*>
    (m .: "route_color" <|> return "FFFFFF") <*>
    (m .: "route_text_color" <|> return "000000") <*>
    (m .: "route_sort_order" <|> return 0)

-- | A single trip as described in the GTFS feed.
data Trip = Trip { tripRouteId :: Identifier Text Route
                 , tripServiceId :: Text
                 , tripId :: Identifier Text Trip
                 , tripHeadsign :: Text
                 , tripShortName :: Text
                 , tripDirectionId :: Maybe Int
                 , tripBlockId :: Text
                 , tripShapeId :: Text
                 , tripWheelchairAccessible :: Maybe Bool
                 , tripBikesAllowed :: Maybe Bool
                 } deriving (Eq, Show, Generic)

instance Identified Text Trip where
  identifier = tripId

instance FromJSON Trip where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Trip where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Trip where
  parseNamedRecord m = Trip <$>
    (Identifier <$> m .: "route_id") <*>
    ({- Identifier <$> -} m .: "service_id") <*>
    (Identifier <$> m .: "trip_id") <*>
    m .: "trip_headsign" <*>
    m .: "trip_short_name" <*>
    (Just <$> m .: "direction_id" <|> return Nothing) <*>
    m .: "block_id" <*>
    m .: "shape_id" <*>
    (maybeBool <$> m .: "wheelchair_accessible" <|> return Nothing) <*>
    (maybeBool <$> m .: "bikes_allowed" <|> return Nothing)
