{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module TellMeT.Server.GTFS where

import           Codec.Archive.Zip    (Archive, findEntryByPath, fromEntry,
                                       toArchiveOrFail)
import           Data.ByteString.Lazy (ByteString, readFile)
import           Data.Csv             (FromNamedRecord, decodeByName)
import           Data.Default         (def)
import           Data.Monoid          ((<>))
import           Lens.Micro           (Lens', (%~))
import           Prelude              hiding (readFile)

import           TellMeT.GTFS         (Feed, agencies, routes, stops, trips)
import           TellMeT.Util         (Identified (Identifier), MapOf, addToMap)

readFeed :: FilePath -> IO (Either String Feed)
readFeed path = do
  zipBytes <- readFile path
  return $ feedFromArchive zipBytes

putMap :: (Identified a, Ord (Identifier a))
       => Lens' t (MapOf a) -> a -> t -> t
putMap part item = part %~ addToMap item

feedFromArchive :: ByteString -> Either String Feed
feedFromArchive zipBytes = do
  archive <- toArchiveOrFail zipBytes
  fs <- sequence [ feedFile True "agency.txt" archive (putMap agencies)
                , feedFile True "stops.txt" archive (putMap stops)
                , feedFile True "routes.txt" archive (putMap routes)
                , feedFile True "trips.txt" archive (putMap trips)
                ]
  return $ foldr (.) id fs $ def

feedFile :: (FromNamedRecord a)
         => Bool -> FilePath -> Archive -> (a -> Feed -> Feed) ->
           Either String (Feed -> Feed)
feedFile required path archive setter =
  case fromEntry <$> findEntryByPath path archive of
    Nothing -> if required
      then Left $ "missing zip file entry " <> path
      else Right id
    Just bs -> do
      (_, records) <- decodeByName bs
      return $ foldr (.) id $ fmap setter records

