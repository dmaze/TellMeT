{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.RouteBadge where

import Data.Monoid ((<>))
import Lens.Micro ((?~), at)
import Miso.Html (Attribute, View, text, class_, span_, style_)
import Miso.String (ms)

import TellMeT.GTFS
  ( Route, routeColor, routeLongName, routeShortName, routeTextColor
  , routeType
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
  let fg = routeTextColor route
      fg' = if fg == "" then id else at "color" ?~ ms ("#" <> fg)
      bg = routeColor route
      bg' = if bg == "" then id else at "background-color" ?~ ms ("#" <> bg)
      attrs = fg' $ bg' $ mempty
  in style_ attrs

viewRouteBadge :: Route -> View action
viewRouteBadge route =
  span_
  [class_ "badge", routeStyle route]
  [viewRouteType $ routeType route, text $ ms $ name]
  where short = routeShortName route
        name = " " <> if short == "" then routeLongName route else short
