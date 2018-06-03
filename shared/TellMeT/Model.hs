{-# LANGUAGE MultiParamTypeClasses #-}

-- | Core data model for the TellMeT application.
module TellMeT.Model (Model, initialModel) where

import           Data.Default                   (def)
import           Data.Map                       (Map)
import           Network.URI                    (URI)

import           TellMeT.Components.FeedFetcher (FeedFetcher (fetchAgencies, fetchRoutes, tripsForRouteFetcher),
                                                 HasFeed (theFeed))
import           TellMeT.Components.Fetcher     (Fetcher)
import           TellMeT.Components.Pages       (OnPage (currentPage))
import           TellMeT.Components.URI         (SiteURI (siteURI))
import           TellMeT.GTFS                   (Agency, Feed, Route, Trip)
import           TellMeT.Pages                  (Page)
import           TellMeT.Util                   (Identifier)

-- | Core data model for the TellMeT application.  The type itself is
-- opaque; all of the data in it can be accessed via its typeclasses.
data Model = Model { _siteUri       :: URI
                   , _currentPage   :: Page
                   , _theFeed       :: Feed
                   , _fetchAgencies :: Fetcher [Agency]
                   , _fetchRoutes   :: Fetcher [Route]
                   , _tripsForRouteFetcher :: Map (Identifier Route) (Fetcher [Trip])
                   } deriving (Eq, Show)

instance SiteURI Model where
  siteURI f m = (\u -> m { _siteUri = u }) <$> f (_siteUri m)

instance OnPage Page Model where
  currentPage f m = (\p -> m { _currentPage = p}) <$> f (_currentPage m)

instance HasFeed Model where
  theFeed f m = (\g -> m { _theFeed = g }) <$> f (_theFeed m)

instance FeedFetcher Model where
  fetchAgencies f m = (\a -> m { _fetchAgencies = a}) <$> f (_fetchAgencies m)
  fetchRoutes f m = (\r -> m { _fetchRoutes = r }) <$> f (_fetchRoutes m)
  tripsForRouteFetcher f m = (\t -> m { _tripsForRouteFetcher = t }) <$>
                             f (_tripsForRouteFetcher m)

-- | Construct an initial (empty) model given the root site URI.
initialModel :: URI -> Model
initialModel uri = Model uri def def def def def

