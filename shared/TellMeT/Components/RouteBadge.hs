{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.RouteBadge where

import Data.Monoid ((<>))
import Lens.Micro ((?~), at)
import Miso.Html (Attribute, View, text, class_, span_, style_)
import Miso.String (ms)

import TellMeT.GTFS
  ( Route, route_color, route_long_name, route_short_name, route_text_color
  , route_type
  , RouteType ( LightRail, Subway, Rail, Bus, Ferry, CableCar, Gondola
              , Funicular
              )
  )

viewRouteType :: RouteType -> View action
viewRouteType LightRail = span_ [class_ "fas fa-subway"] []
viewRouteType Subway = span_ [class_ "fas fa-subway"] []
viewRouteType Rail = span_ [class_ "fas fa-train"] []
viewRouteType Bus = span_ [class_ "fas fa-bus"] []
viewRouteType Ferry = span_ [class_ "fas fa-ship"] []
viewRouteType CableCar = span_ [class_ "fas fa-subway"] []
viewRouteType Gondola = span_ [class_ "fas fa-subway"] []
viewRouteType Funicular = span_ [class_ "fas fa-subway"] []

routeStyle :: Route -> Attribute action
routeStyle route =
  let fg = route_text_color route
      fg' = if fg == "" then id else at "color" ?~ ms ("#" <> fg)
      bg = route_color route
      bg' = if bg == "" then id else at "background-color" ?~ ms ("#" <> bg)
      attrs = fg' $ bg' $ mempty
  in style_ attrs

viewRouteBadge :: Route -> View action
viewRouteBadge route =
  span_
  [class_ "badge", routeStyle route]
  [viewRouteType $ route_type route, text $ ms $ name]
  where short = route_short_name route
        name = " " <> if short == "" then route_long_name route else short
