{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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

import           Control.Applicative             ((<|>))
import           Control.Monad                   (guard)
import           Data.Aeson.Types                (Options (fieldLabelModifier),
                                                  camelTo2, defaultOptions)
#ifndef __GHCJS__
import           Data.Aeson                      (genericParseJSON,
                                                  genericToJSON, withText)
import           Data.Aeson.Types                (FromJSON (parseJSON),
                                                  ToJSON (toJSON))
#endif
import           Data.Char                       (digitToInt, isDigit)
import           Data.Csv                        (FromField (parseField), FromNamedRecord (parseNamedRecord),
                                                  (.:))
import           Data.Default                    (Default (def))
import           Data.List                       (sortOn)
import           Data.Monoid                     ((<>))
import           Data.Text.Encoding              (decodeLatin1)
import           Data.Time.Calendar              (Day, fromGregorian)
import           Data.Time.Format                (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime             (TimeOfDay (TimeOfDay))
import           GHC.Generics                    (Generic)
#ifdef __GHCJS__
import           JavaScript.JSON.Types.Generic   ()
import           JavaScript.JSON.Types.Instances (FromJSON (parseJSON),
                                                  ToJSON (toJSON),
                                                  genericParseJSON,
                                                  genericToJSON, withJSString)
import           JavaScript.JSON.Types.Internal  (Parser, Value)
#endif
import           Lens.Micro                      (at, (%~))
import           Lens.Micro.GHC                  ()
import           Miso.String                     (MisoString, fromMisoString,
                                                  toMisoString)
import           Web.HttpApiData                 (FromHttpApiData,
                                                  ToHttpApiData)

import           TellMeT.Util                    (Identified (Identifier, identifier),
                                                  MapOf)

#ifdef __GHCJS__
withText :: String -> (MisoString -> Parser a) -> Value -> Parser a
withText = withJSString
#endif

-- | Data extracted from an entire GTFS feed.
data Feed = Feed { _agencies :: MapOf Agency
                 , _stops    :: MapOf Stop
                 , _routes   :: MapOf Route
                 , _trips    :: MapOf Trip
                 , _services :: MapOf Service
                 } deriving (Eq, Show)

instance Default Feed where
  def = Feed { _agencies = mempty
             , _stops = mempty
             , _routes = mempty
             , _trips = mempty
             , _services = mempty
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

-- | Add a 'StopTime' to its associated trip, or ignore it if the
-- trip does not exist.
putStopTime :: StopTime -> Feed -> Feed
putStopTime st = trips . at (stopTimeTripId st) %~ \case
  Nothing -> Nothing
  Just trip -> Just $ trip { tripStopTimes = sortOn stopTimeStopSequence $
                                            tripStopTimes trip <> [st] }

-- | Access the map of trip ID to service for a feed.
services :: (Functor t) => (MapOf Service -> t (MapOf Service)) -> Feed -> t Feed
services f feed = (\s -> feed { _services = s }) <$> f (_services feed)

-- | Add a 'Calendar' to a feed, creating its 'Service' if needed.
putCalendar :: Calendar -> Feed -> Feed
putCalendar cal = services . at (calendarServiceId cal) %~ \case
  Nothing -> Just $ Service { serviceId = calendarServiceId cal
                           , serviceCalendar = Just cal
                           , serviceDates = []
                           }
  Just service -> Just $ service { serviceCalendar = Just cal }

-- | Add a 'CalendarDate' to a feed, creating its 'Service' if needed.
putCalendarDate :: CalendarDate -> Feed -> Feed
putCalendarDate cd = services . at (calendarDateServiceId cd) %~ \case
  Nothing -> Just $ Service { serviceId = calendarDateServiceId cd
                           , serviceCalendar = Nothing
                           , serviceDates = [cd]
                           }
  Just service -> Just $ service { serviceDates = serviceDates service <> [cd] }

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
data Agency = Agency { agencyId       :: !(Identifier Agency)
                     , agencyName     :: !MisoString
                     , agencyUrl      :: !MisoString
                     , agencyTimeZone :: !MisoString
                     , agencyLang     :: !(Maybe MisoString)
                     , agencyPhone    :: !(Maybe MisoString)
                     , agencyFareUrl  :: !(Maybe MisoString)
                     , agencyEmail    :: !(Maybe MisoString)
                     } deriving (Eq, Show, Generic)

instance Identified Agency where
  newtype Identifier Agency = AgencyIdentifier MisoString
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
  parseJSON = withText "LocationType" $ \case
    "stop"     -> return StopType
    "station"  -> return Station
    "entrance" -> return Entrance
    _          -> fail "invalid LocationType"
instance ToJSON LocationType where
  toJSON StopType = toJSON ("stop" :: MisoString)
  toJSON Station  = toJSON ("station" :: MisoString)
  toJSON Entrance = toJSON ("entrance" :: MisoString)

-- | A place where some transit vehicle stops.
data Stop = Stop { stopId             :: !(Identifier Stop)
                 , stopCode           :: !MisoString
                 , stopName           :: !MisoString
                 , stopDesc           :: !MisoString
                 , stopLat            :: !Float
                 , stopLon            :: !Float
                 , zoneId             :: !MisoString
                 , stopUrl            :: !MisoString
                 , locationType       :: !LocationType
                 , parentStation      :: !MisoString
                 , stopTimezone       :: !MisoString
                 , wheelchairBoarding :: !(Maybe Bool)
                 } deriving (Eq, Show, Generic)

instance Identified Stop where
  newtype Identifier Stop = StopIdentifier MisoString
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
  parseJSON = withText "RouteType" $ \case
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
  toJSON LightRail = toJSON ("light_rail" :: MisoString)
  toJSON Subway    = toJSON ("subway" :: MisoString)
  toJSON Rail      = toJSON ("rail" :: MisoString)
  toJSON Bus       = toJSON ("bus" :: MisoString)
  toJSON Ferry     = toJSON ("ferry" :: MisoString)
  toJSON CableCar  = toJSON ("cable_car" :: MisoString)
  toJSON Gondola   = toJSON ("gondola" :: MisoString)
  toJSON Funicular = toJSON ("funicular" :: MisoString)

-- | A single transit route as described in a GTFS feed.
data Route = Route { routeId        :: !(Identifier Route)
                   , routeAgencyId  :: !(Identifier Agency)
                   , routeShortName :: !MisoString
                   , routeLongName  :: !MisoString
                   , routeDesc      :: !MisoString
                   , routeType      :: !RouteType
                   , routeUrl       :: !MisoString
                   , routeColor     :: !MisoString
                   , routeTextColor :: !MisoString
                   , routeSortOrder :: !Int
                   } deriving (Eq, Show, Generic)

instance Identified Route where
  newtype Identifier Route = RouteIdentifier MisoString
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
data Trip = Trip { tripRouteId              :: !(Identifier Route)
                 , tripServiceId            :: !(Identifier Service)
                 , tripId                   :: !(Identifier Trip)
                 , tripHeadsign             :: !MisoString
                 , tripShortName            :: !MisoString
                 , tripDirectionId          :: !(Maybe Int)
                 , tripBlockId              :: !MisoString
                 , tripShapeId              :: !MisoString
                 , tripWheelchairAccessible :: !(Maybe Bool)
                 , tripBikesAllowed         :: !(Maybe Bool)
                 , tripStopTimes            :: ![StopTime]
                 } deriving (Eq, Show, Generic)

instance Identified Trip where
  newtype Identifier Trip = TripIdentifier MisoString
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
    (ServiceIdentifier <$> m .: "service_id") <*>
    (TripIdentifier <$> m .: "trip_id") <*>
    (m .: "trip_headsign" <|> return "") <*>
    (m .: "trip_short_name" <|> return "") <*>
    (Just <$> m .: "direction_id" <|> return Nothing) <*>
    (m .: "block_id" <|> return "") <*>
    (m .: "shape_id" <|> return "") <*>
    (maybeBool <$> m .: "wheelchair_accessible" <|> return Nothing) <*>
    (maybeBool <$> m .: "bikes_allowed" <|> return Nothing) <*>
    return []

-- | The type of a pickup or drop-off associated with a specific stop.
data PickupType = Scheduled | NotAvailable | PhoneAgency | ContactDriver
  deriving (Eq, Ord, Enum, Show)
instance FromJSON PickupType where
  parseJSON = withText "PickupType" $ \case
    "scheduled"      -> return Scheduled
    "not_available"  -> return NotAvailable
    "phone_agency"   -> return PhoneAgency
    "contact_driver" -> return ContactDriver
    _                -> fail "invalid PickupType"
instance ToJSON PickupType where
  toJSON Scheduled     = toJSON ("scheduled" :: MisoString)
  toJSON NotAvailable  = toJSON ("not_available" :: MisoString)
  toJSON PhoneAgency   = toJSON ("phone_agency" :: MisoString)
  toJSON ContactDriver = toJSON ("contact_driver" :: MisoString)

-- | The time when a StopTime occurs.  This is in the form of seconds
-- since "midnight", where "midnight" is defined as 12 hours before
-- noon (which may not be actual midnight due to daylight savings).
-- Its JSON serialization is in HH:MM:SS form, matching what is
-- allowed in the GTFS CSV format.
newtype StopTimeTime = StopTimeTime Int deriving (Eq, Ord, Show)

instance FromJSON StopTimeTime where
  parseJSON = withText "StopTimeTime" (parseStopTimeTime . toMisoString)

instance ToJSON StopTimeTime where
  toJSON = toJSON . showStopTimeTime

instance FromField StopTimeTime where
  parseField = parseStopTimeTime . toMisoString . decodeLatin1

-- | Make a stop-time time from hours, minutes, and seconds.
hmsToStopTimeTime :: Int -> Int -> Int -> StopTimeTime
hmsToStopTimeTime h m s = StopTimeTime $ (h * 60 + m) * 60 + s

-- | Convert a time-of-day structure to a stop-time time.
todToStopTimeTime :: TimeOfDay -> StopTimeTime
todToStopTimeTime (TimeOfDay hours minutes seconds) =
  hmsToStopTimeTime hours minutes (round seconds)

-- | Parse a text blob into a stop-time time.  The text blob should
-- look like "HH:MM:SS" or "H:MM:SS".  Fails in the monad if the
-- format is wrong.
parseStopTimeTime :: (Monad m) => MisoString -> m StopTimeTime
parseStopTimeTime tt =
  case parseIt $ fromMisoString tt of
    Nothing  -> fail $ fromMisoString $ "invalid time " <> tt
    Just stt -> return stt
  where num c = if isDigit c then Just (digitToInt c) else Nothing
        uncons (c:cs) = Just (c, cs)
        uncons _      = Nothing
        unconsNum (c:cs) = do n <- num c
                              return (n, cs)
        unconsNum _ = Nothing
        colon (':':cs) = return cs
        colon _        = Nothing
        twoDigit tens ones = tens * 10 + ones
        parseIt t0 = do
          (h1, t1) <- unconsNum t0
          (h2c, t2) <- uncons t1
          (h, t3) <- if h2c == ':'
                    then return (h1, t2)
                    else do h2 <- num h2c
                            t3' <- colon t2
                            return (twoDigit h1 h2, t3')
          (m1, t4) <- unconsNum t3
          (m2, t5) <- unconsNum t4
          let m = twoDigit m1 m2
          t6 <- colon t5
          (s1, t7) <- unconsNum t6
          (s2, t8) <- unconsNum t7
          let s = twoDigit s1 s2
          guard $ t8 == []
          return $ hmsToStopTimeTime h m s

-- | Convert a stop-time time (in seconds since midnight) to a
-- time-of-day structure.  We use this so that we can have a more
-- memory-efficient storage, since there are many many stop-times.
-- Note that this can produce technically invalid times of day: a
-- service that runs past midnight will have a GTFS time after
-- 24:00:00 which is technically out of bounds.
stopTimeTimeToTod :: StopTimeTime -> TimeOfDay
stopTimeTimeToTod (StopTimeTime manySeconds) =
  let (manyMinutes, seconds) = divMod manySeconds 60
      (hours, minutes) = divMod manyMinutes 60
  in TimeOfDay hours minutes (fromIntegral seconds)

-- | Convert a stop-time time to a printable string, in the form
-- "HH:MM:SS".
showStopTimeTime :: StopTimeTime -> MisoString
showStopTimeTime = toMisoString .
                   formatTime defaultTimeLocale "%T" .
                   stopTimeTimeToTod

-- | A single stop on a single trip.
data StopTime = StopTime { stopTimeTripId            :: !(Identifier Trip)
                         , stopTimeArrivalTime       :: !(Maybe StopTimeTime)
                         , stopTimeDepartureTime     :: !(Maybe StopTimeTime)
                         , stopTimeStopId            :: !(Identifier Stop)
                         , stopTimeStopSequence      :: !Int
                         , stopTimeStopHeadsign      :: !MisoString
                         , stopTimePickupType        :: !PickupType
                         , stopTimeDropOffType       :: !PickupType
                         , stopTimeShapeDistTraveled :: !(Maybe Float)
                         , stopTimeTimepoint         :: !Bool
                         } deriving (Eq, Show, Generic)

instance FromJSON StopTime where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON StopTime where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord StopTime where
  parseNamedRecord m = StopTime <$>
    (TripIdentifier <$> m .: "trip_id") <*>
    m .: "arrival_time" <*>
    m .: "departure_time" <*>
    (StopIdentifier <$> m .: "stop_id") <*>
    m .: "stop_sequence" <*>
    m .: "stop_headsign" <*>
    ((toEnum <$> m .: "pickup_type") <|> return Scheduled) <*>
    ((toEnum <$> m .: "drop_off_type") <|> return Scheduled) <*>
    (m .: "shape_dist_traveled" <|> return Nothing) <*>
    (((/= (0 :: Int)) <$> m .: "timepoint") <|> return True)

-- | A set of dates when a particular schedule could apply, like
-- "weekdays".  This doesn't correspond to a concrete GTFS object,
-- but trips, calendar blocks, and specific dates all refer to
-- services.
data Service = Service { serviceId       :: !(Identifier Service)
                       , serviceCalendar :: !(Maybe Calendar)
                       , serviceDates    :: ![CalendarDate]
                       } deriving (Eq, Show, Generic)

instance Identified Service where
  newtype Identifier Service = ServiceIdentifier MisoString
    deriving (Eq, Show, Ord, FromJSON, ToJSON, FromHttpApiData,
              ToHttpApiData)
  identifier = serviceId

instance FromJSON Service where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Service where
  toJSON = genericToJSON jsonOptions

instance Default Service where
  def = Service (ServiceIdentifier "") Nothing []

-- | Create a service description from an extant calendar.
serviceFromCalendar :: Calendar -> Service
serviceFromCalendar c = Service (calendarServiceId c) (Just c) []

-- | Create a service description from a single calendar date.
serviceFromCalendarDate :: CalendarDate -> Service
serviceFromCalendarDate cd = Service (calendarDateServiceId cd) Nothing [cd]

-- | Specific dates, as formatted in the GTFS feed.
newtype CalendarDay = CalendarDay Day deriving (Eq, Show, Ord)

instance FromJSON CalendarDay where
  parseJSON = withText "CalendarDay" (parseCalendarDay . toMisoString)

instance ToJSON CalendarDay where
  toJSON = toJSON . showCalendarDay

instance FromField CalendarDay where
  parseField = parseCalendarDay . toMisoString . decodeLatin1

-- | Parse a text blob "YYYYMMDD" into a date.  Fails in the monad if
-- the date is not valid.
parseCalendarDay :: (Monad m) => MisoString -> m CalendarDay
parseCalendarDay s =
  let digits = fromMisoString s
      check b = if b then return () else fail $ "invalid date " <> digits
      nn = digitToInt <$> digits
      yyyy = (nn !! 0) * 1000 + (nn !! 1) * 100 + (nn !! 2) * 10 + (nn !! 3)
      mm = (nn !! 4) * 10 + (nn !! 5)
      dd = (nn !! 6) * 10 + (nn !! 7)
  in do check $ length digits == 8
        check $ all isDigit digits
        return $ CalendarDay $ fromGregorian (fromIntegral yyyy) mm dd

-- | Serialize a date into a text blob "YYYYMMDD".
showCalendarDay :: CalendarDay -> MisoString
showCalendarDay (CalendarDay d) =
  toMisoString $ formatTime defaultTimeLocale "%0Y%0m%0d" d

-- | A generic day-of-week-based schedule.  The service runs
-- on the specified days of the week, from the specified start
-- date through the specified end date.  A feed is specified
-- to contain at most one calendar record per service ID.
data Calendar = Calendar { calendarServiceId :: !(Identifier Service)
                         , calendarMonday    :: !Bool
                         , calendarTuesday   :: !Bool
                         , calendarWednesday :: !Bool
                         , calendarThursday  :: !Bool
                         , calendarFriday    :: !Bool
                         , calendarSaturday  :: !Bool
                         , calendarSunday    :: !Bool
                         , calendarStartDate :: !CalendarDay
                         , calendarEndDate   :: !CalendarDay
                         } deriving (Eq, Show, Generic)

instance Identified Calendar where
  newtype Identifier Calendar = CalendarIdentifier (Identifier Service)
  identifier = CalendarIdentifier . calendarServiceId

instance FromJSON Calendar where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Calendar where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord Calendar where
  parseNamedRecord m = Calendar <$>
    (ServiceIdentifier <$> m .: "service_id") <*>
    ((/= (0 :: Int)) <$> m .: "monday") <*>
    ((/= (0 :: Int)) <$> m .: "tuesday") <*>
    ((/= (0 :: Int)) <$> m .: "wednesday") <*>
    ((/= (0 :: Int)) <$> m .: "thursday") <*>
    ((/= (0 :: Int)) <$> m .: "friday") <*>
    ((/= (0 :: Int)) <$> m .: "saturday") <*>
    ((/= (0 :: Int)) <$> m .: "sunday") <*>
    m .: "start_date" <*>
    m .: "end_date"

-- | Whether a 'CalendarDate' exception is an additional day of
-- a service or a day that does not have service.
data ExceptionType = NoException | Added | Removed
  deriving (Eq, Show, Enum)

instance FromJSON ExceptionType where
  parseJSON = withText "ExceptionType " $ \case
    "no_exception" -> return NoException
    "added"        -> return Added
    "removed"      -> return Removed
    _              -> fail "invalid ExceptionType"

instance ToJSON ExceptionType where
  toJSON NoException = toJSON ("no_exception" :: MisoString)
  toJSON Added       = toJSON ("added" :: MisoString)
  toJSON Removed     = toJSON ("removed" :: MisoString)

-- | An exception to scheduled service on some specific date.  This is
-- tied to a service ID.  It may specify days a service does or does
-- not run ("this service is weekdays only, but not Memorial Day";
-- "this service is weekends only, plus Memorial Day").  A service may
-- also have no 'Calendar' record but only have date records, in which
-- case it only runs on a specific set of dates.
data CalendarDate = CalendarDate { calendarDateServiceId :: !(Identifier Service)
                                 , calendarDateDate :: !CalendarDay
                                 , calendarDateExceptionType :: !ExceptionType
                                 } deriving (Eq, Show, Generic)

instance Identified CalendarDate where
  data Identifier CalendarDate =
    CalendarDateIdentifier (Identifier Service) CalendarDay
  identifier cd = CalendarDateIdentifier
                  (calendarDateServiceId cd)
                  (calendarDateDate cd)

instance FromJSON CalendarDate where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CalendarDate where
  toJSON = genericToJSON jsonOptions

instance FromNamedRecord CalendarDate where
  parseNamedRecord m = CalendarDate <$>
    (ServiceIdentifier <$> m .: "service_id") <*>
    m .: "date" <*>
    (toEnum <$> m .: "exception_type")
