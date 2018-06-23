-- |Update the model in response to an action.
module TellMeT.UI.Update where

import           Control.Monad.Writer.Class     (tell)
import           Data.Default                   (def)
import           Lens.Micro                     (at, (.~))
import           Lens.Micro.Mtl                 (use, (.=))
import           Miso.Types                     (Transition)
import           TellMeT.Components.FeedFetcher (FeedFetchAction,
                                                 fetchedAgencies, fetchedRoutes,
                                                 fetchedServices,
                                                 fetchedTripsForRoute,
                                                 ifFetchFeed,
                                                 ifFetchTripsForRoute,
                                                 ifFetchedAgencies,
                                                 ifFetchedRoutes,
                                                 ifFetchedServices,
                                                 ifFetchedTripsForRoute)
import           TellMeT.GTFS                   (Agency, Feed, Route, Service,
                                                 agencies, routes, services)
import           TellMeT.Model.Feed             (FeedFetcher, HasFeed,
                                                 fetchAgencies, fetchRoutes,
                                                 fetchServices, theFeed,
                                                 tripsForRouteFetcher)
import           TellMeT.Model.Fetcher          (Fetcher (Fetched, Fetching))
import           TellMeT.REST                   (linkAgencies, linkRoutes,
                                                 linkServices,
                                                 linkTripsForRoute)
import           TellMeT.UI.Fetcher             (fetch)
import           TellMeT.Util                   (addToMap)

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
