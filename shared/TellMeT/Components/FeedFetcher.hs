{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.FeedFetcher where

import           Data.Monoid           ((<>))
import           Lens.Micro            ((^.))
import           Miso.Html             (View, div_, p_, text)
import           Miso.String           (MisoString, ms)

import           TellMeT.Bootstrap     (fa_)
import           TellMeT.GTFS          (Agency, Route, Service, Trip)
import           TellMeT.Model.Feed    (FeedFetcher, fetchAgencies, fetchRoutes,
                                        fetchServices, haveFeed)
import           TellMeT.Model.Fetcher (Fetcher (FetchFailed, Fetched, Fetching, Unfetched))
import           TellMeT.Util          (Identifier)

class FeedFetchAction action where
  fetchFeed :: action
  fetchedAgencies :: Fetcher [Agency] -> action
  fetchedRoutes :: Fetcher [Route] -> action
  fetchedServices :: Fetcher [Service] -> action
  fetchTripsForRoute :: Identifier Route -> action
  fetchedTripsForRoute :: Identifier Route
                       -> Fetcher [Trip]
                       -> action

-- | If we have the base feed already, run some other view function;
-- otherwise ignore the view function and fetch the feed.
viewOrFetch :: (FeedFetcher model) => model -> View action -> View action
viewOrFetch m v = if haveFeed m then v else viewFeedFetch m

viewAFetch :: (Show obj) => MisoString -> Fetcher obj -> View action
viewAFetch title Unfetched = p_ []
  [ fa_ "circle"
  , text title
  ]
viewAFetch title Fetching = p_ []
  [ fa_ "circle-notch fa-spin"
  , text title
  ]
viewAFetch title (FetchFailed msg) = p_ []
  [ fa_ "times-circle"
  , text (title <> ": " <> msg)
  ]
viewAFetch title (Fetched obj) = p_ []
  [ fa_ "check-circle"
  , text (title <> ": " <> (ms $ show $ obj))
  ]

viewFeedFetch :: (FeedFetcher model) => model -> View action
viewFeedFetch model = div_ []
  [ viewAFetch "Agencies" $ model ^. fetchAgencies
  , viewAFetch "Routes" $ model ^. fetchRoutes
  , viewAFetch "Services" $ model ^. fetchServices
  ]
