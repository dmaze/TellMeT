{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | URIs and Servant handlers for pages in the application.
module TellMeT.Pages where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Miso.Html (View)
import Network.URI (URI)
import Servant.API (Capture, (:>), (:<|>), safeLink)

import TellMeT.Action (Action)
import TellMeT.GTFS (Route)
import TellMeT.Util (Identifier)

-- | All Servant routes for application pages.
type ViewRoutes = Home :<|> ARoute

-- | Servant type for the home (route-list) page.
type Home = View Action

-- | A link to the home (route-list) page.
linkHome :: URI
linkHome = safeLink (Proxy @ViewRoutes) (Proxy @Home)

-- | Servant type for the route details page.
type ARoute = "route" :> Capture "id" (Identifier Text Route) :> View Action

-- | A link to the route details page for a specific route.
linkRoute :: Identifier Text Route -> URI
linkRoute = safeLink (Proxy @ViewRoutes) (Proxy @ARoute)
