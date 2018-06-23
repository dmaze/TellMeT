-- |Models that hold or can fetch GTFS feeds.
module TellMeT.Model.Feed (HasFeed(..), FeedFetcher(..), haveFeed) where

import           Data.Map              (Map)
import           Lens.Micro            (Lens', (^.))
import           TellMeT.GTFS          (Agency, Feed, Identifier, Route,
                                        Service, Trip)
import           TellMeT.Model.Fetcher (Fetcher (Fetched))

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

-- |Determine whether we have fetched all of the parts of the feed
-- that get fetched at startup time.
haveFeed :: (FeedFetcher model) => model -> Bool
haveFeed model = do
  case (model ^. fetchAgencies, model ^. fetchRoutes,
        model ^. fetchServices) of
    (Fetched _, Fetched _, Fetched _) -> True
    _                                 -> False
