{-# LANGUAGE MultiParamTypeClasses #-}

-- |Type classes for actions.
module TellMeT.Action.Class where

import           Network.URI           (URI)

import           TellMeT.GTFS          (Agency, Route, Service, Trip)
import           TellMeT.Model.Fetcher (Fetcher)
import           TellMeT.Util          (Identifier)

-- |Actions that can affect the current site URI.
class URIAction action where
  -- |An action that causes the site URI to change.  This does not
  -- cause an immediate change in the model; instead, it changes the
  -- browser URL, and waits for a handleURIChange action to be
  -- produced.
  changeURI :: URI -> action

  -- |An action taken in response to the site URI changing.  This
  -- action probably wants to be fired in response to an
  -- application-global subscription using Miso's 'uriSub' function.
  handleURIChange :: URI -> action

-- |Actions that can act on the current page.
--
-- There should be one top-level page router that responds to
-- the 'goToPage' action by invoking the browser history API, and that
-- translates top-level page URI changes into 'nowOnPage' actions.
class PageAction page action {- | action -> page -} where
  -- |Create an action to go to a specified page.
  goToPage :: page -> action

  -- |Create an action to go to a specified page, and also return the
  -- target of a link that goes there.
  goToPageLink :: page -> (action, URI)

-- |Actions related to fetching parts of the GTFS feed.
class FeedFetchAction action where
  -- |Create an action to fetch the entire feed.
  fetchFeed :: action

  -- |Create an action to update the fetch status of the top-level
  -- list of agencies.
  fetchedAgencies :: Fetcher [Agency] -> action

  -- |Create an action to update the fetch status of the top-level
  -- list of routes.
  fetchedRoutes :: Fetcher [Route] -> action

  -- |Create an action to update the fetch status of the top-level
  -- list of services.
  fetchedServices :: Fetcher [Service] -> action

  -- |Create an action to fetch the trips for a specific route.
  fetchTripsForRoute :: Identifier Route -> action

  -- |Create an action to update the fetch status of the list
  -- of trips for a specific route.
  fetchedTripsForRoute :: Identifier Route
                       -> Fetcher [Trip]
                       -> action
