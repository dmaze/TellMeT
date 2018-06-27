{-# LANGUAGE TypeApplications #-}

-- |Update the model in response to an action.
module TellMeT.UI.Update (updateModel) where

import           Control.Monad                      (void)
import           Control.Monad.State.Class          (get)
import           Control.Monad.Writer.Class         (tell)
import           Data.Default                       (def)
import           Data.Proxy                         (Proxy (Proxy))
import qualified Data.Set                           as Set
import qualified JavaScript.Object                  as Object
import           Lens.Micro                         (at, each, (.~), (^?))
import           Lens.Micro.Mtl                     (use, view, (.=), (<%=))
import           Miso.Effect                        (Effect)
import           Miso.Html                          (VTree (VTree),
                                                     View (View, runView))
import           Miso.Router                        (runRoute)
import           Miso.Subscription.History          (pushURI)
import           Miso.Types                         (Transition, fromTransition)
import           Servant.API                        ((:<|>) ((:<|>)))
import           TellMeT.Action                     (Action (..), ViewRoutes,
                                                     pageLink)
import           TellMeT.Components.DirectionPicker (pickedDirection)
import           TellMeT.Components.Pages           (currentPage)
import           TellMeT.Components.RoutePage       (filterByService,
                                                     visibleServices)
import           TellMeT.Components.ServicePicker   (pickedService)
import           TellMeT.Components.URI             (siteURI)
import           TellMeT.GTFS                       (Agency, Feed, Route,
                                                     Service, Trip, agencies,
                                                     routes, services,
                                                     tripDirectionId)
import           TellMeT.Model                      (Model)
import           TellMeT.Model.Feed                 (fetchAgencies, fetchRoutes,
                                                     fetchServices, theFeed,
                                                     tripsForRouteFetcher)
import           TellMeT.Model.Fetcher              (Fetcher (Fetched, Fetching, Unfetched))
import           TellMeT.Pages                      (Page (NoPage, RouteList, RoutePage))
import           TellMeT.REST                       (linkAgencies, linkRoutes,
                                                     linkServices,
                                                     linkTripsForRoute)
import           TellMeT.UI.Fetcher                 (fetch)
import           TellMeT.Util                       (Identifier, addToMap)

-- | Apply the effects of an action to the model, producing an updated
-- model and possibly asynchronous actions.
updateModel :: Action -> Model -> Effect Action Model
updateModel = fromTransition . doAction

doAction :: Action -> Transition Action Model ()
doAction NoOp = return ()
doAction (ChangeURI uri) = tell [\_ -> pushURI uri]
doAction (HandleURIChange uri) = do
  siteURI .= uri
  let pageTree = dispatchOnPage RouteList :<|>
                 dispatchOnPage . RoutePage
  model <- get
  let view' = runRoute (Proxy @ViewRoutes) pageTree (view siteURI) model
  let theView = either (dispatchOnPage NoPage) id view'
  tell [void . runView theView]
doAction (GoToPage page) = tell [\_ -> pushURI $ pageLink page]
doAction (NowOnPage page) = do
  currentPage .= page
  case page of
    RoutePage routeId -> onRoutePage routeId
    _                 -> return ()
doAction FetchFeed = do
  fetchAgencies .= Fetching
  fetchRoutes .= Fetching
  fetchServices .= Fetching
  tell [ \d -> fetch linkAgencies >>= d . FetchedAgencies
       , \d -> fetch linkRoutes >>= d . FetchedRoutes
       , \d -> fetch linkServices >>= d . FetchedServices
       ]
doAction (FetchedAgencies as) = do
  fetchAgencies .= as
  buildFeed
doAction (FetchedRoutes rs) = do
  fetchRoutes .= rs
  buildFeed
doAction (FetchedServices ss) = do
  fetchServices .= ss
  buildFeed
doAction (FetchTripsForRoute routeId) = do
  tripsForRouteFetcher . at routeId .= Just Fetching
  tell [ \dispatch -> do
           trips <- fetch (linkTripsForRoute routeId)
           dispatch $ FetchedTripsForRoute routeId trips ]
doAction (FetchedTripsForRoute routeId fetchr) = do
  tripsForRouteFetcher . at routeId .= Just fetchr
  case fetchr of
    Fetched trips -> do aService <- updatePickedService trips
                        _ <- updatePickedDirection trips aService
                        return ()
    _ -> return ()
doAction (PickService s) = pickedService .= s
doAction (PickDirection d) = pickedDirection .= d

buildFeed :: Transition Action Model ()
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

-- We need to decode the page URL matching a Servant route.
-- This looks mildly tricky and kind of annoying.  There are at least
-- two canned implementations out there already: one in a standalone
-- servant-matcher package (which requires Servant 0.13) and the one
-- in Miso.Router.
--
-- Miso.Router, though, basically only works when the routes end
-- in (View action) and you're matching it against an object with
-- terminals of type (model -> View action).  That means we need
-- this fake view:

-- | Create an artificial view that fires a now-on-page action
-- for a specific page.
dispatchOnPage :: Page -> model -> View Action
dispatchOnPage page _ = dispatch (NowOnPage page)
  where dispatch action = View $ \d -> d action >> VTree <$> Object.create

onRoutePage :: Identifier Route
            -> Transition Action Model ()
onRoutePage routeId = do
  fetcher <- use $ tripsForRouteFetcher . at routeId
  case maybe Unfetched id fetcher of
    Unfetched -> tell [ \d -> d $ FetchTripsForRoute routeId ]
    _         -> return ()

-- | Given a list of newly visible trips, update the chosen
-- service if necessary.
updatePickedService :: [Trip]
                    -> Transition Action Model (Maybe (Identifier Service))
updatePickedService trips =
  let someServices = visibleServices trips
      anyService = someServices ^? each
      pickValidService Nothing = anyService
      pickValidService (Just sid) =
        if sid `elem` someServices
        then Just sid
        else anyService
  in pickedService <%= pickValidService

-- | Given a list of newly visible trips and a chosen service,
-- update the chosen direction if necessary.
updatePickedDirection :: [Trip]
                      -> Maybe (Identifier Service)
                      -> Transition Action Model (Maybe Int)
updatePickedDirection trips aService =
  let trips' = filterByService aService trips
      directions = foldMap (Set.singleton . tripDirectionId) trips'
      pickValidDirection dir =
        if dir `Set.member` directions
        then dir
        else if Set.null directions
             then Nothing
             else Set.findMin directions
  in pickedDirection <%= pickValidDirection
