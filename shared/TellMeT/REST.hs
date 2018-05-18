{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module TellMeT.REST where

import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Network.URI (URI)
import Servant.API (Capture, Get, JSON, (:>), (:<|>) (..), safeLink)
import TellMeT.GTFS (Agency, Stop, Route)

type MapAPI a = Get '[JSON] [a] :<|> Capture "id" Text :> Get '[JSON] a

type RestAPI = "api" :> ("agency" :> MapAPI Agency :<|>
                         "stop" :> MapAPI Stop :<|>
                         "route" :> MapAPI Route)

linkAgencies :: URI
linkAgencies = safeLink (Proxy @RestAPI)
               (Proxy @("api" :> "agency" :> Get '[JSON] [Agency]))

linkRoutes :: URI
linkRoutes = safeLink (Proxy @RestAPI)
             (Proxy @("api" :> "route" :> Get '[JSON] [Route]))
