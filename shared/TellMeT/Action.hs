{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Core action type.
module TellMeT.Action where

import           Data.Default                   (Default (def))
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Text                      (Text)
import           Network.URI                    (URI)

#ifdef __GHCJS__
import           Control.Monad                  (void)
import           Data.Either                    (either)
import qualified JavaScript.Object              as Object
import           Lens.Micro.Mtl                 ((.=))
import           Miso.Html                      (VTree (VTree),
                                                 View (View, runView))
import           Miso.Router                    (runRoute)
import           Miso.Types                     (Transition, scheduleSub)
import           Servant.API                    ((:<|>) ((:<|>)), (:>), Capture,
                                                 safeLink)
#else
import           Miso.Html                      (View)
import           Servant.API                    ((:<|>), (:>), Capture,
                                                 safeLink)
#endif

import           TellMeT.Components.FeedFetcher (FeedFetchAction (fetchFeed, fetchTripsForRoute, fetchedAgencies, fetchedRoutes, fetchedTripsForRoute, ifFetchFeed, ifFetchTripsForRoute, ifFetchedAgencies, ifFetchedRoutes, ifFetchedTripsForRoute),
                                                 FeedFetcher)
import           TellMeT.Components.Fetcher     (Fetcher)
import           TellMeT.Components.Pages       (OnPage (currentPage), PageAction (goToPage, goToPageLink, ifGoToPage, ifNowOnPage, nowOnPage))
import           TellMeT.Components.URI         (URIAction (changeURI, handleURIChange, ifChangeURI, ifHandleURIChange))
import           TellMeT.GTFS                   (Agency, Route, Trip)
import           TellMeT.Pages                  (Page (NoPage, RouteList, RoutePage))
import           TellMeT.Util                   (Identifier)
#ifdef __GHCJS__
import           TellMeT.Components.RoutePage   (onRoutePage)
#endif

-- | The concrete type of an action.
--
-- Components shouldn't use this directly.  Instead they should
-- declare a typeclass for specific things they need out of the action
-- (usually creating some specific action type and running an
-- arbitrary monadic action if the action type matches), and this type
-- will implement that typeclass.
data Action
    -- | Don't do anything.
  = NoOp
    -- | Change the page URI using the browser history API.
  | ChangeURI URI
    -- | Respond to the page URI changing, including from a "ChangeURI"
    -- action.
  | HandleURIChange URI
    -- | Go to a specific enumerated page.
  | GoToPage Page
    -- | An announcement that we have arrived on some page.
  | NowOnPage Page
    -- | Request to fetch the global feed data.
  | FetchFeed
    -- | An announcement that we have fetched the list of agencies.
  | FetchedAgencies (Fetcher [Agency])
    -- | An announcement that we have fetched the list of routes.
  | FetchedRoutes (Fetcher [Route])
    -- | Request to fetch the list of trips for a specific route.
    | FetchTripsForRoute (Identifier Text Route)
    -- | Announce that we have received the list of trips for a route.
    | FetchedTripsForRoute (Identifier Text Route) (Fetcher [Trip])
  deriving (Show, Eq)

instance Default Action where
  def = NoOp

instance URIAction Action where
  changeURI = ChangeURI
  ifChangeURI (ChangeURI uri) f = f uri
  ifChangeURI _ _               = return ()
  handleURIChange = HandleURIChange
  ifHandleURIChange (HandleURIChange uri) f = f uri
  ifHandleURIChange _ _                     = return ()

instance FeedFetchAction Action where
  fetchFeed = FetchFeed
  ifFetchFeed FetchFeed a = a
  ifFetchFeed _ _         = return ()
  fetchedAgencies = FetchedAgencies
  ifFetchedAgencies (FetchedAgencies ff) a = a ff
  ifFetchedAgencies _ _                    = return ()
  fetchedRoutes = FetchedRoutes
  ifFetchedRoutes (FetchedRoutes ff) a = a ff
  ifFetchedRoutes _ _                  = return ()
  fetchTripsForRoute = FetchTripsForRoute
  ifFetchTripsForRoute (FetchTripsForRoute r) f = f r
  ifFetchTripsForRoute _ _                      = return ()
  fetchedTripsForRoute = FetchedTripsForRoute
  ifFetchedTripsForRoute (FetchedTripsForRoute r t) f = f r t
  ifFetchedTripsForRoute _ _                          = return ()

instance PageAction Page Action where
  goToPage = GoToPage
  goToPageLink p = (GoToPage p, pageLink p)
  ifGoToPage (GoToPage p) f = f p
  ifGoToPage _ _            = return ()
  nowOnPage = NowOnPage
  ifNowOnPage (NowOnPage p) f = f p
  ifNowOnPage _ _             = return ()

-- The page-link wiring winds up here.  We have the unfortunate
-- dependency chain that the Servant types depend on the concrete
-- Action type, and generating a link depends on the Servant types, so
-- where the rest of the component machinery tries very carefully to
-- be independent of any specific action representation, if a Page can
-- generate its own URI then you have to import Action along the way
-- to get it.
--
-- There might be a better way to parameterize this, but I still don't
-- quite understand the Servant types.

-- | Get the actual link URI for some page.
pageLink :: Page -> URI
pageLink NoPage = pageLink RouteList -- shouldn't really go here
pageLink RouteList = safeLink (Proxy @ViewRoutes)
                     (Proxy @(RouteListRoute))
pageLink (RoutePage rid) = safeLink (Proxy @ViewRoutes)
                           (Proxy @(RoutePageRoute)) rid

-- | Servant type for the route-list page.
type RouteListRoute = View Action

-- | Servant type for the route details page.
type RoutePageRoute = "route"
                      :> Capture "id" (Identifier Text Route)
                      :> View Action

-- | All Servant routes for viewing application pages.
type ViewRoutes = RouteListRoute :<|> RoutePageRoute

#ifdef __GHCJS__
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

-- | Artificial view that dispatches the action given.
dispatch :: action -> View action
dispatch action = View $ \sink -> sink action >> VTree <$> Object.create

-- | Create an artificial view that fires a now-on-page action
-- for a specific page.
dispatchOnPage :: (PageAction Page action) => Page -> model -> View action
dispatchOnPage page _ = dispatch $ nowOnPage page

-- | Forward requests to go to specific pages, and observe when we are
-- on a specific page.
updatePage :: (OnPage Page model, FeedFetcher model,
              FeedFetchAction action, PageAction Page action, URIAction action)
           => action
           -> Transition action model ()
updatePage action = do
  ifGoToPage action $ \page ->
    scheduleSub $ \sink -> sink $ changeURI $ pageLink page
  ifHandleURIChange action $ \uri ->
    let pageTree = dispatchOnPage RouteList :<|>
                   \p -> dispatchOnPage (RoutePage p)
        view = either (dispatchOnPage NoPage) id $
               runRoute (Proxy @ViewRoutes) pageTree id uri
    in scheduleSub $ void . runView view
  ifNowOnPage action $ \page -> do
    currentPage .= page
    case page of
      RoutePage routeId -> onRoutePage routeId
      _                 -> return ()
#endif
