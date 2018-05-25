{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Core client page routing.
module TellMeT.Routes where

import Data.Either (either)
import Data.Proxy (Proxy(Proxy))
import Lens.Micro.Extras (view)
import Miso (View, runRoute, text)
import Network.URI (URI)
import Servant.API (safeLink)

import TellMeT.Action (Action)
import TellMeT.Components.Chrome (viewChrome)
import TellMeT.Components.FeedFetcher (haveFeed, viewFeedFetch)
import TellMeT.Components.RouteList (viewRouteList)
import TellMeT.Components.URI (siteURI)
import TellMeT.Model (Model)

type ViewRoutes = Home

type Home = View Action

viewModel :: Model -> View Action
viewModel m =
  either (\_ -> view404) id $
  runRoute (Proxy @ViewRoutes) viewTree (view siteURI) m

viewTree :: (Model -> View Action)
viewTree = viewHome

viewHome :: Model -> View Action
viewHome m = viewChrome linkHome inner
  where inner = if haveFeed m
                then viewRouteList m
                else viewFeedFetch m

linkHome :: URI
linkHome = safeLink (Proxy @ViewRoutes) (Proxy @Home)

view404 :: View a
view404 = text "not found"
