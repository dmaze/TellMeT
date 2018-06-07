{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.FeedFetcher where

import           Data.Map                   (Map)
import           Data.Monoid                ((<>))
import           Lens.Micro                 (Lens', at, (.~), (^.))
import           Lens.Micro.GHC             ()
import           Miso.Html                  (View, div_, p_, text)
import           Miso.String                (MisoString, ms)

import           TellMeT.Bootstrap          (fa_)
import           TellMeT.Components.Fetcher (Fetcher (FetchFailed, Fetched, Fetching, Unfetched))
import           TellMeT.GTFS               (Agency, Feed, Route,Service, Trip,
                                             agencies, routes, services)
import           TellMeT.Util               (Identifier, addToMap)

#ifdef __GHCJS__
import           Control.Monad.Writer.Class (tell)
import           Data.Default               (def)
import           Lens.Micro.Mtl             (use, (.=))
import           Miso.Types                 (Transition)
import           TellMeT.Components.Fetcher (fetch)
import           TellMeT.REST               (linkAgencies, linkRoutes, linkServices,
                                             linkTripsForRoute)
#endif

class HasFeed model where
  theFeed :: Lens' model Feed

class FeedFetcher model where
  fetchAgencies :: Lens' model (Fetcher [Agency])
  fetchRoutes :: Lens' model (Fetcher [Route])
  fetchServices :: Lens' model (Fetcher [Service])
  tripsForRouteFetcher :: Lens' model (Map (Identifier Route) (Fetcher [Trip]))

class FeedFetchAction action where
  fetchFeed :: action
  ifFetchFeed :: (Monad m) => action -> m () -> m ()
  fetchedAgencies :: Fetcher [Agency] -> action
  ifFetchedAgencies :: (Monad m)
                    => action -> (Fetcher [Agency] -> m ()) -> m ()
  fetchedRoutes :: Fetcher [Route] -> action
  ifFetchedRoutes :: (Monad m)
                  => action -> (Fetcher [Route] -> m ()) -> m ()
  fetchedServices :: Fetcher [Service] -> action
  ifFetchedServices :: (Monad m)
                    => action -> (Fetcher [Service] -> m ()) -> m ()
  fetchTripsForRoute :: Identifier Route -> action
  ifFetchTripsForRoute :: (Monad m)
                       => action -> (Identifier Route -> m ()) -> m ()
  fetchedTripsForRoute :: Identifier Route
                       -> Fetcher [Trip]
                       -> action
  ifFetchedTripsForRoute :: (Monad m)
                         => action
                         -> (Identifier Route -> Fetcher [Trip] -> m ())
                         -> m ()

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

haveFeed :: (FeedFetcher model) => model -> Bool
haveFeed model = do
  case (model ^. fetchAgencies, model ^. fetchRoutes, \
           model ^. fetchServices) of
    (Fetched _, Fetched _, Fetched _) -> True
    _                      -> False

#ifdef __GHCJS__
updateFeedFetch :: (HasFeed model, FeedFetcher model, FeedFetchAction action)
                => action
                -> Transition action model ()
updateFeedFetch a = do
  ifFetchFeed a $ do
    fetchAgencies .= Fetching
    fetchRoutes .= Fetching
    fetchServices .= Fetching
    tell [ \dispatch -> fetch linkAgencies >>= dispatch . fetchedAgencies
         , \dispatch -> fetch linkRoutes >>= dispatch . fetchedRoutes
         , \dispatch -> fetch linkServices >>= dispatch . fetchedServices
         ]
    return ()
  ifFetchedAgencies a $ \as -> do
    fetchAgencies .= as
    buildFeed
  ifFetchedRoutes a $ \rs -> do
    fetchRoutes .= rs
    buildFeed
  ifFetchedServices a $ \ss -> do
    fetchServices .= ss
    buildFeed
  ifFetchTripsForRoute a $ \routeId -> do
    tripsForRouteFetcher . at routeId .= Just Fetching
    tell [ \dispatch -> do
             trips <- fetch (linkTripsForRoute routeId)
             dispatch $ fetchedTripsForRoute routeId trips ]
  ifFetchedTripsForRoute a $ \routeId trips -> do
    tripsForRouteFetcher . at routeId .= Just trips
  return ()

buildFeed :: (HasFeed model, FeedFetcher model) => Transition action model ()
buildFeed = do
  fAgencies <- use fetchAgencies
  fRoutes <- use fetchRoutes
  fServices <- use fetchServices
  case (fAgencies, fRoutes, fServices) of
    (Fetched theAgencies, Fetched theRoutes, Fetched theServices) ->
      theFeed .= buildFeedFrom theAgencies theRoutes theServices
    _ -> return ()

buildFeedFrom :: [Agency] -> [Route] -> [Service] -> Feed
buildFeedFrom theAgencies theRoutes theServices =
  agencies .~ foldr addToMap def theAgencies $
  routes .~ foldr addToMap def theRoutes $
  services .~ foldr addToMap def theServices $
  def
#endif
