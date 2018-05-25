{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Core client page routing.
module TellMeT.Routes where

import Data.Either (either)
import Data.Proxy (Proxy(Proxy))
import Lens.Micro.Extras (view)
import Miso (View, runRoute, text)
import Servant.API ((:<|>) ((:<|>)))

import TellMeT.Action (Action)
import TellMeT.Components.Chrome (viewChrome)
import TellMeT.Components.FeedFetcher (viewOrFetch)
import TellMeT.Components.RouteList (viewRouteList)
import TellMeT.Components.RoutePage (viewRoutePage)
import TellMeT.Components.URI (siteURI)
import TellMeT.Model (Model)
import TellMeT.Pages (ViewRoutes, linkHome)

viewModel :: Model -> View Action
viewModel m =
  viewChrome linkHome $
  either (\_ -> view404) (viewOrFetch m) $
  runRoute (Proxy @ViewRoutes) viewTree (view siteURI) m
  where viewTree = viewRouteList :<|> viewRoutePage

view404 :: View a
view404 = text "not found"
