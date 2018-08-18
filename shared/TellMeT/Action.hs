{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

-- | Core action type.
module TellMeT.Action where

import           Data.Default          (Default (def))
import           Data.Proxy            (Proxy (Proxy))
import           Network.URI           (URI)

import           Miso.Html             (View)
import           Servant.API           ((:<|>), (:>), Capture, safeLink)
import           TellMeT.Action.Class  (FeedFetchAction (fetchFeed, fetchTripsForRoute, fetchedAgencies, fetchedRoutes, fetchedServices, fetchedTripsForRoute),
                                        PageAction (goToPage, goToPageLink),
                                        URIAction (changeURI, handleURIChange))
import           TellMeT.GTFS          (Agency, Route, Service, Trip)
import           TellMeT.Model.Fetcher (Fetcher)
import           TellMeT.Pages         (Page (NoPage, RouteList, RoutePage))
import           TellMeT.Util          (Identifier)

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
    -- | An announcement that we have fetched the list of services.
  | FetchedServices (Fetcher [Service])
    -- | Request to fetch the list of trips for a specific route.
  | FetchTripsForRoute (Identifier Route)
    -- | Announce that we have received the list of trips for a route.
  | FetchedTripsForRoute (Identifier Route) (Fetcher [Trip])
    -- | Pick a specific service, to filter by day-of-week.
  | PickService (Maybe (Identifier Service))
    -- | Pick a specific direction.
  | PickDirection (Maybe Int)
  deriving (Show, Eq)

instance Default Action where
  def = NoOp

instance URIAction Action where
  changeURI = ChangeURI
  handleURIChange = HandleURIChange

instance FeedFetchAction Action where
  fetchFeed = FetchFeed
  fetchedAgencies = FetchedAgencies
  fetchedRoutes = FetchedRoutes
  fetchedServices = FetchedServices
  fetchTripsForRoute = FetchTripsForRoute
  fetchedTripsForRoute = FetchedTripsForRoute

instance PageAction Page Action where
  goToPage = GoToPage
  goToPageLink p = (GoToPage p, pageLink p)

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
                      :> Capture "id" (Identifier Route)
                      :> View Action

-- | All Servant routes for viewing application pages.
type ViewRoutes = RouteListRoute :<|> RoutePageRoute
