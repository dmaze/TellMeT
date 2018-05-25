{-# LANGUAGE OverloadedStrings #-}

-- | Display a page for a given route.
module TellMeT.Components.RoutePage where

import Data.Text (Text)
import Lens.Micro ((^.), at)
import Miso.Html (View, text)

import TellMeT.Bootstrap (alert_, fa_)
import TellMeT.Components.FeedFetcher (HasFeed (theFeed))
import TellMeT.Components.RouteBadge (viewRouteBadge)
import TellMeT.Components.URI (URIAction)
import TellMeT.GTFS (Route, routes)
import TellMeT.Util (Identifier)

-- | Display the page for a given route.  Note that the identifier will
-- come out of the URL, not out of the model state.
viewRoutePage :: (HasFeed model, URIAction action)
              => Identifier Text Route -> model -> View action
viewRoutePage routeId model = case model ^. theFeed . routes . at routeId of
  Nothing -> viewNoRoutePage
  Just route -> viewARoutePage route

-- | Display an error page for an invalid route ID.
viewNoRoutePage :: View action
viewNoRoutePage =
  alert_ "danger" [fa_ "frown", text $ "Uh oh, I don't know that route."]

viewARoutePage :: (URIAction action) => Route -> View action
viewARoutePage route =
  viewRouteBadge route
