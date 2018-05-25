{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.RouteList where

import Data.List (sortOn)
import Data.Monoid ((<>))
import Lens.Micro ((^..), each)
import Lens.Micro.GHC ()
import Miso.Html (View, text)
import Miso.Html.Element (li_, ul_)
import Miso.Html.Property (class_)
import Miso.String (ms)

import TellMeT.GTFS
  ( Route, routes
  , routeDesc, routeLongName, routeShortName, routeSortOrder
  )
import TellMeT.Components.FeedFetcher (HasFeed (theFeed))
import TellMeT.Components.RouteBadge (viewRouteBadge)
import TellMeT.Components.URI (URIAction)

viewOneRoute :: (URIAction action) => Route -> View action
viewOneRoute route = li_ [] [viewRouteBadge route, text $ ms $ desc]
  where short = routeShortName route
        long = routeLongName route
        moreName = if short /= "" && long /= "" then long else ""
        theDesc = routeDesc route
        sep = if moreName /= "" && theDesc /= "" then ": " else ""
        desc = " " <> moreName <> sep <> theDesc

viewAllRoutes :: (URIAction action) => [Route] -> View action
viewAllRoutes theRoutes =
  ul_ [class_ "list-unstyled"] $
  viewOneRoute <$> sortOn routeSortOrder theRoutes

viewRouteList :: (HasFeed model, URIAction action) => model -> View action
viewRouteList model = viewAllRoutes (model ^.. theFeed . routes . each)
