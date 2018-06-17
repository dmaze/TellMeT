{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Core data model for the TellMeT application.
module TellMeT.Model (Model, initialModel) where

import           Data.Default                       (def)
import           Data.Map                           (Map)
import           Network.URI                        (URI)

import           TellMeT.Components.DirectionPicker (PickedDirection (pickedDirection))
import           TellMeT.Components.FeedFetcher     (FeedFetcher (fetchAgencies, fetchRoutes, fetchServices, tripsForRouteFetcher),
                                                     HasFeed (theFeed))
import           TellMeT.Components.Fetcher         (Fetcher)
import           TellMeT.Components.Pages           (OnPage (currentPage))
import           TellMeT.Components.ServicePicker   (PickedService (pickedService))
import           TellMeT.Components.URI             (SiteURI (siteURI))
import           TellMeT.GTFS                       (Agency, Feed, Route,
                                                     Service, Trip)
import           TellMeT.Pages                      (Page)
import           TellMeT.Util                       (Identifier)

-- | Core data model for the TellMeT application.  The type itself is
-- opaque; all of the data in it can be accessed via its typeclasses.
data Model = Model { _siteUri       :: URI
                   , _currentPage   :: Page
                   , _theFeed       :: Feed
                   , _fetchAgencies :: Fetcher [Agency]
                   , _fetchRoutes   :: Fetcher [Route]
                   , _fetchServices :: Fetcher [Service]
                   , _tripsForRouteFetcher :: Map (Identifier Route) (Fetcher [Trip])
                   , _pickedService :: Maybe (Identifier Service)
                   , _pickedDirection :: Maybe Int
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
  fetchServices f m = (\s -> m { _fetchServices = s }) <$> f (_fetchServices m)
  tripsForRouteFetcher f m = (\t -> m { _tripsForRouteFetcher = t }) <$>
                             f (_tripsForRouteFetcher m)

instance PickedService Model where
  pickedService f m = (\s -> m { _pickedService = s }) <$> f (_pickedService m)

instance PickedDirection Model where
  pickedDirection f m = (\d -> m { _pickedDirection = d }) <$> f (_pickedDirection m)

-- | Construct an initial (empty) model given the root site URI.
initialModel :: URI -> Model
initialModel uri = Model uri def def def def def def def def

