module TellMeT.Model where

import Data.Default (def)
import Lens.Micro (Lens')
import Network.URI (URI)

import TellMeT.Components.FeedFetcher
  ( HasFeed (theFeed)
  , FeedFetcher (fetchAgencies, fetchRoutes)
  )
import TellMeT.Components.Fetcher (Fetcher)
import TellMeT.GTFS (Agency, Feed, Route)

data Model = Model { _siteUri :: URI
                   , _theFeed :: Feed
                   , _fetchAgencies :: Fetcher [Agency]
                   , _fetchRoutes :: Fetcher [Route]
                   } deriving (Eq, Show)

siteUri :: Lens' Model URI
siteUri f m = (\u -> m { _siteUri = u }) <$> f (_siteUri m)

instance HasFeed Model where
  theFeed f m = (\g -> m { _theFeed = g }) <$> f (_theFeed m)

instance FeedFetcher Model where
  fetchAgencies f m = (\a -> m { _fetchAgencies = a}) <$> f (_fetchAgencies m)
  fetchRoutes f m = (\r -> m { _fetchRoutes = r }) <$> f (_fetchRoutes m)

initialModel :: URI -> Model
initialModel uri = Model uri def def def

