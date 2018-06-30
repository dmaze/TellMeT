{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type classes for the TellMeT data model.
module TellMeT.Model.Class where

import           Data.Map              (Map)
import           Lens.Micro            (Lens')
import           Network.URI           (URI)
import           TellMeT.GTFS          (Agency, Feed, Identifier, Route,
                                        Service, Trip)
import           TellMeT.Model.Fetcher (Fetcher)

-- |Models that are aware of the current site URI.
class SiteURI model where
  -- |Access the current site URI.
  siteURI :: Lens' model URI

-- |Models that hold (possibly partial) GTFS feeds.
class HasFeed model where
  -- |Access the feed stored in the model.  The expectation is that
  -- this may not be a complete feed; for instance, the feed may have
  -- routes with no matching trips.  Correspondingly the feed may be
  -- updated over time as more of it is fetched.
  theFeed :: Lens' model Feed

-- |Models that can fetch parts of GTFS feeds.
class FeedFetcher model where
  -- |Access the fetch state for the list of agencies.
  fetchAgencies :: Lens' model (Fetcher [Agency])

  -- |Access the fetch state for the list of routes.
  fetchRoutes :: Lens' model (Fetcher [Route])

  -- |Access the fetch state for the list of services.
  fetchServices :: Lens' model (Fetcher [Service])

  -- |Access the fetch state for specific routes' trips.
  tripsForRouteFetcher :: Lens' model (Map (Identifier Route) (Fetcher [Trip]))

-- |Models that know what page we're on.
class OnPage page model where
  -- |Access the current page.
  currentPage :: Lens' model page

-- |Models that may have a single service chosen.
class PickedService model where
  -- |Access the currently-chosen service.  'Nothing' means no
  -- service is chosen.
  pickedService :: Lens' model (Maybe (Identifier Service))

-- |Models that have a single direction chosen.  Trips, in
-- particular, have directions.  GTFS specifies that the "direction"
-- column is optional but that if it is present it should be 0 or 1,
-- so hopefully this is consistent across routes (and if not then
-- Nothing is a sensible choice probably).
class PickedDirection model where
  -- |Access the currently-picked direction.  'Nothing' is a valid
  -- direction.
  pickedDirection :: Lens' model (Maybe Int)
