{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- | Data representation of the General Transit Feed Specification.
--
-- This module has mostly straight ports of Google's GTFS CSV file
-- formats; see <https://developers.google.com/transit/gtfs/>.  It
-- includes instances for 'FromNamedRecord' to parse the GTFS files,
-- and 'FromJSON' and 'ToJSON' to provide a REST interface.
module TellMeT.GTFS where

import           Control.Applicative ((<|>))
import           Data.Aeson          (genericParseJSON, genericToJSON, withText)
import           Data.Aeson.Types    (FromJSON (parseJSON),
                                      Options (fieldLabelModifier),
                                      ToJSON (toJSON), camelTo2, defaultOptions)
import           Data.Csv            (FromNamedRecord (parseNamedRecord), (.:))
import           Data.Default        (Default (def))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)
import           Web.HttpApiData     (FromHttpApiData, ToHttpApiData)

import           TellMeT.Util        (Identified (Identifier, identifier),
                                      MapOf)

-- | Data extracted from an entire GTFS feed.
data Feed = Feed { _agencies :: MapOf Agency
                 , _stops    :: MapOf Stop
                 , _routes   :: MapOf Route
                 , _trips    :: MapOf Trip
                 } deriving (Eq, Show)

instance Default Feed where
  def = Feed { _agencies = mempty
             , _stops = mempty
             , _routes = mempty
             , _trips = mempty
             }

-- | Access the map of agency ID to agency for a feed.
agencies :: (Functor t) => (MapOf Agency -> t (MapOf Agency)) -> Feed -> t Feed
agencies f feed = (\a -> feed { _agencies = a }) <$> f (_agencies feed)

-- | Access the map of stop ID to stop for a feed.
stops :: (Functor t) => (MapOf Stop -> t (MapOf Stop)) -> Feed -> t Feed
stops f feed = (\s -> feed { _stops = s }) <$> f (_stops feed)

-- | Access the map of route ID to route for a feed.
routes :: (Functor t) => (MapOf Route -> t (MapOf Route)) -> Feed -> t Feed
routes f feed = (\r -> feed { _routes = r }) <$> f (_routes feed)

-- | Access the map of trip ID to trip for a feed.
trips :: (Functor t) => (MapOf Trip -> t (MapOf Trip)) -> Feed -> t Feed
trips f feed = (\t -> feed { _trips = t}) <$> f (_trips feed)

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
data Agency = Agency { agencyId       :: Identifier Agency
                     , agencyName     :: Text
                     , agencyUrl      :: Text
                     , agencyTimeZone :: Text
                     , agencyLang     :: Maybe Text
                     , agencyPhone    :: Maybe Text
                     , agencyFareUrl  :: Maybe Text
                     , agencyEmail    :: Maybe Text
                     } deriving (Eq, Show, Generic)

instance Identified Agency where
  newtype Identifier Agency = AgencyIdentifier Text
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromHttpApiData,
              ToHttpApiData)
  identifier = agencyId

instance FromJSON Agency where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Agency where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Agency where
  parseNamedRecord m = Agency <$>
    (AgencyIdentifier <$> (m .: "agency_id" <|> return "")) <*>
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
      "stop"     -> return StopType
      "station"  -> return Station
      "entrance" -> return Entrance
      _          -> fail "invalid LocationType"
instance ToJSON LocationType where
  toJSON StopType = toJSON ("stop" :: Text)
  toJSON Station  = toJSON ("station" :: Text)
  toJSON Entrance = toJSON ("entrance" :: Text)

-- | A place where some transit vehicle stops.
data Stop = Stop { stopId             :: Identifier Stop
                 , stopCode           :: Text
                 , stopName           :: Text
                 , stopDesc           :: Text
                 , stopLat            :: Float
                 , stopLon            :: Float
                 , zoneId             :: Text
                 , stopUrl            :: Text
                 , locationType       :: LocationType
                 , parentStation      :: Text
                 , stopTimezone       :: Text
                 , wheelchairBoarding :: Maybe Bool
                 } deriving (Eq, Show, Generic)

instance Identified Stop where
  newtype Identifier Stop = StopIdentifier Text
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromHttpApiData,
              ToHttpApiData)
  identifier = stopId

instance FromJSON Stop where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Stop where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Stop where
  parseNamedRecord m = Stop <$>
    (StopIdentifier <$> m .: "stop_id") <*>
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
      "subway"     -> return Subway
      "rail"       -> return Rail
      "bus"        -> return Bus
      "ferry"      -> return Ferry
      "cable_car"  -> return CableCar
      "gondola"    -> return Gondola
      "funicular"  -> return Funicular
      _            -> fail "invalid RouteType"
instance ToJSON RouteType where
  toJSON LightRail = toJSON ("light_rail" :: Text)
  toJSON Subway    = toJSON ("subway" :: Text)
  toJSON Rail      = toJSON ("rail" :: Text)
  toJSON Bus       = toJSON ("bus" :: Text)
  toJSON Ferry     = toJSON ("ferry" :: Text)
  toJSON CableCar  = toJSON ("cable_car" :: Text)
  toJSON Gondola   = toJSON ("gondola" :: Text)
  toJSON Funicular = toJSON ("funicular" :: Text)

-- | A single transit route as described in a GTFS feed.
data Route = Route { routeId        :: Identifier Route
                   , routeAgencyId  :: Identifier Agency
                   , routeShortName :: Text
                   , routeLongName  :: Text
                   , routeDesc      :: Text
                   , routeType      :: RouteType
                   , routeUrl       :: Text
                   , routeColor     :: Text
                   , routeTextColor :: Text
                   , routeSortOrder :: Int
                   } deriving (Eq, Show, Generic)

instance Identified Route where
  newtype Identifier Route = RouteIdentifier Text
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromHttpApiData,
              ToHttpApiData)
  identifier = routeId

instance FromJSON Route where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Route where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Route where
  parseNamedRecord m = Route <$>
    (RouteIdentifier <$> m .: "route_id") <*>
    (AgencyIdentifier <$> (m .: "agency_id" <|> return "")) <*>
    m .: "route_short_name" <*>
    m .: "route_long_name" <*>
    (m .: "route_desc" <|> return "") <*>
    (toEnum <$> m .: "route_type") <*>
    (m .: "route_url" <|> return "") <*>
    (m .: "route_color" <|> return "FFFFFF") <*>
    (m .: "route_text_color" <|> return "000000") <*>
    (m .: "route_sort_order" <|> return 0)

-- | A single trip as described in the GTFS feed.
data Trip = Trip { tripRouteId              :: Identifier Route
                 , tripServiceId            :: Text
                 , tripId                   :: Identifier Trip
                 , tripHeadsign             :: Text
                 , tripShortName            :: Text
                 , tripDirectionId          :: Maybe Int
                 , tripBlockId              :: Text
                 , tripShapeId              :: Text
                 , tripWheelchairAccessible :: Maybe Bool
                 , tripBikesAllowed         :: Maybe Bool
                 } deriving (Eq, Show, Generic)

instance Identified Trip where
  newtype Identifier Trip = TripIdentifier Text
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromHttpApiData,
              ToHttpApiData)
  identifier = tripId

instance FromJSON Trip where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Trip where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Trip where
  parseNamedRecord m = Trip <$>
    (RouteIdentifier <$> m .: "route_id") <*>
    ({- Identifier <$> -} m .: "service_id") <*>
    (TripIdentifier <$> m .: "trip_id") <*>
    m .: "trip_headsign" <*>
    m .: "trip_short_name" <*>
    (Just <$> m .: "direction_id" <|> return Nothing) <*>
    m .: "block_id" <*>
    m .: "shape_id" <*>
    (maybeBool <$> m .: "wheelchair_accessible" <|> return Nothing) <*>
    (maybeBool <$> m .: "bikes_allowed" <|> return Nothing)
