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
  , route_desc, route_long_name, route_short_name, route_sort_order
  )
import TellMeT.Components.FeedFetcher (HasFeed (theFeed))
import TellMeT.Components.RouteBadge (viewRouteBadge)

viewOneRoute :: Route -> View action
viewOneRoute route = li_ [] [viewRouteBadge route, text $ ms $ desc]
  where short = route_short_name route
        long = route_long_name route
        more_name = if short /= "" && long /= "" then long else ""
        the_desc = route_desc route
        sep = if more_name /= "" && the_desc /= "" then ": " else ""
        desc = " " <> more_name <> sep <> the_desc

viewAllRoutes :: [Route] -> View action
viewAllRoutes theRoutes =
  ul_ [class_ "list-unstyled"] $
  viewOneRoute <$> sortOn route_sort_order theRoutes

viewRouteList :: (HasFeed model) => model -> View action
viewRouteList model = viewAllRoutes (model ^.. theFeed . routes . each)
