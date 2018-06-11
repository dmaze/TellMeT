{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module TellMeT.Server.GTFS where

import           Codec.Archive.Zip          (Archive, findEntryByPath,
                                             fromEntry, toArchiveOrFail)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.State.Strict (MonadState, execStateT)
import           Data.ByteString.Lazy       (ByteString, readFile)
import           Data.Csv                   (FromNamedRecord)
import           Data.Csv.Streaming         (Records (Cons, Nil), decodeByName)
import           Data.Default               (def)
import           Data.List                  (sortOn)
import           Data.Map.Strict            (Map, alter, mergeWithKey)
import qualified Data.Map.Strict            as Map
import           Data.Monoid                ((<>))
import           Lens.Micro                 (Lens', (%~))
import           Lens.Micro.Mtl             (assign)
import           Prelude                    hiding (readFile)

import           TellMeT.GTFS               (CalendarDate, Feed, Service, Stop,
                                             StopTime, Trip, agencies,
                                             calendarDateServiceId, routes,
                                             serviceDates, serviceFromCalendar,
                                             serviceFromCalendarDate, services,
                                             stopTimeStopId,
                                             stopTimeStopSequence,
                                             stopTimeTripId, stops,
                                             tripStopTimes, trips)
import           TellMeT.Util               (Identified (Identifier), MapOf,
                                             addToMap, identifier)

readFeed :: FilePath -> IO (Either String Feed)
readFeed path = do
  zipBytes <- readFile path
  return $ feedFromBytes zipBytes

putMap :: (Identified a, Ord (Identifier a))
       => Lens' t (MapOf a) -> a -> t -> t
putMap part item = part %~ addToMap item

feedFromBytes :: ByteString -> Either String Feed
feedFromBytes zipBytes = do
  archive <- toArchiveOrFail zipBytes
  execStateT (feedFromArchive archive) def

feedFromArchive :: (MonadState Feed m, MonadError String m) => Archive -> m ()
feedFromArchive archive = do
  feedFile True "agency.txt" archive >>= toMap >>= assign agencies
  theStops <- feedFile True "stops.txt" archive >>= toMap
  assign stops theStops
  feedFile True "routes.txt" archive >>= toMap >>= assign routes
  theTrips <- feedFile True "trips.txt" archive >>= toMap
  feedFile True "stop_times.txt" archive >>= putStopTimes theStops theTrips >>= assign trips
  theServices <- fmap serviceFromCalendar <$> feedFile True "calendar.txt" archive >>= toMap
  feedFile False "calendar_dates.txt" archive >>= putCalendarDates theServices >>= assign services

-- | Read a file out of an archive into a lazy record sequence.
feedFile :: (FromNamedRecord a, MonadError String m)
         => Bool -> FilePath -> Archive -> m (Records a)
feedFile required path archive =
  case fromEntry <$> findEntryByPath path archive of
    Nothing -> if required
      then throwError $ "missing zip file entry " <> path
      else return $ Nil Nothing mempty
    Just bs -> case decodeByName bs of
      Left err           -> throwError err
      Right (_, records) -> return records

-- | Fold a lazy record sequence into a final state.
foldRecords :: (MonadError String m) => (a -> s -> s) -> s -> Records a -> m s
foldRecords _ _ (Cons (Left err) _) = throwError err
foldRecords f s (Cons (Right x) xs) = s `seq` foldRecords f (f x s) xs
foldRecords _ _ (Nil (Just err) _)  = throwError err
foldRecords _ s (Nil (Nothing) _)   = s `seq` return s

-- | Turn a lazy record sequence into a strict map.
toMap :: (Identified a, Ord (Identifier a), MonadError String m)
      => Records a -> m (MapOf a)
toMap = buildMap mempty

-- | Add the records from a lazy record sequence into a map.
buildMap :: (Identified a, Ord (Identifier a), MonadError String m)
         => MapOf a -> Records a -> m (MapOf a)
buildMap = foldRecords addToMap

-- | Group the records from a lazy record sequence into a map by
-- some selected key.  The objects in the map will be in reverse
-- order compared to the feed data.
groupRecords :: (Ord k, MonadError String m)
             => (a -> k) -> Records a -> m (Map k [a])
groupRecords f = foldRecords groupOne mempty
  where groupOne x = alter (\l -> Just (x:maybe [] id l)) (f x)

-- | Add the records from a lazy record sequence of stop times to their
-- corresponding trips.  If a stop time names a trip that does not exist,
-- it is ignored.
putStopTimes :: (MonadError String m)
             => MapOf Stop -> MapOf Trip -> Records StopTime -> m (MapOf Trip)
putStopTimes theStops theTrips records = do
  let reusify r = r
        { stopTimeTripId = reuseIdentifier theTrips $ stopTimeTripId r
        , stopTimeStopId = reuseIdentifier theStops $ stopTimeStopId r
        }
  let records' = reusify <$> records
  grouped <- groupRecords stopTimeTripId records'
  let sorted = sortOn stopTimeStopSequence <$> grouped
  return $ mergeWithKey
    (\_ trip theSTs -> Just trip { tripStopTimes = theSTs })
    id (const mempty) theTrips sorted

-- | Add the records from a lazy record sequence of calendar dates
-- to their corresponding services, creating new services if needed.
putCalendarDates :: (MonadError String m)
                 => MapOf Service -> Records CalendarDate -> m (MapOf Service)
putCalendarDates = foldRecords $ \cd ->
  alter
  (\case
      Nothing -> Just (serviceFromCalendarDate cd)
      Just s -> Just (s { serviceDates = serviceDates s <> [cd] }))
  (calendarDateServiceId cd)

-- | If an identifier exists in a map, use the saved identifier.
-- This helps reduce memory utilization when there are a large number
-- of objects that contain object references, especially for StopTime
-- records.
reuseIdentifier :: (Identified a, Ord (Identifier a))
                => MapOf a -> Identifier a -> Identifier a
reuseIdentifier objs anId =
  case Map.lookup anId objs of
    Nothing  -> anId
    Just obj -> identifier obj
