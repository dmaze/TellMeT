{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.RouteBadge where

import Data.Monoid ((<>))
import Lens.Micro ((?~), at)
import Miso.Event.Decoder (emptyDecoder)
import Miso.Html (Attribute, View, text, class_, style_)
import Miso.Html.Element (a_, span_)
import Miso.Html.Event (Options (preventDefault, stopPropagation),
                        defaultOptions, onWithOptions)
import Miso.Html.Property (href_)
import Miso.String (ms)

import TellMeT.Bootstrap (fa_)
import TellMeT.Components.URI (URIAction (changeURI))
import TellMeT.GTFS
  ( Route, routeColor, routeId, routeLongName, routeShortName, routeTextColor
  , routeType
  , RouteType ( LightRail, Subway, Rail, Bus, Ferry, CableCar, Gondola
              , Funicular
              )
  )
import TellMeT.Pages (linkRoute)

viewRouteType :: RouteType -> View action
viewRouteType LightRail = fa_ "subway"
viewRouteType Subway = fa_ "subway"
viewRouteType Rail = fa_ "train"
viewRouteType Bus = fa_ "bus"
viewRouteType Ferry = fa_ "ship"
viewRouteType CableCar = fa_ "subway"
viewRouteType Gondola = fa_ "subway"
viewRouteType Funicular = fa_ "subway"

routeStyle :: Route -> Attribute action
routeStyle route =
  let fg = routeTextColor route
      fg' = if fg == "" then id else at "color" ?~ ms ("#" <> fg)
      bg = routeColor route
      bg' = if bg == "" then id else at "background-color" ?~ ms ("#" <> bg)
      attrs = fg' $ bg' $ mempty
  in style_ attrs

viewRouteBadge :: (URIAction action) => Route -> View action
viewRouteBadge route =
  span_
  [ class_ "badge"
  , routeStyle route
  ]
  [ a_
    [ href_ $ ms $ "/" <> show target
    , routeStyle route
    , onClick $ changeURI target
    ]
    [ viewRouteType $ routeType route, text $ ms $ name]
  ]
  where onClick a = onWithOptions (defaultOptions { preventDefault = True
                                                  , stopPropagation = True })
                    "click" emptyDecoder (const a)
        target = linkRoute $ routeId route
        short = routeShortName route
        name = " " <> if short == "" then routeLongName route else short
