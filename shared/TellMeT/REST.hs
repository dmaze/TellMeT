{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module TellMeT.REST where

import           Data.Proxy   (Proxy (Proxy))
import           Network.URI  (URI)
import           Servant.API  ((:<|>) (..), (:>), Capture, Get, JSON, safeLink)
import           TellMeT.GTFS (Agency, Route, Service, Stop, Trip)
import           TellMeT.Util (Identifier)

type MapAPI a = Get '[JSON] [a] :<|>
                Capture "id" (Identifier a) :> Get '[JSON] a

type RouteTripsAPI = Capture "id" (Identifier Route) :> "trips" :> Get '[JSON] [Trip]

type RouteAPI = MapAPI Route :<|> RouteTripsAPI


type RestAPI = "api" :> ("agency" :> MapAPI Agency :<|>
                         "route" :> RouteAPI :<|>
                         "service" :> MapAPI Service :<|>
                         "stop" :> MapAPI Stop :<|>
                         "trip" :> MapAPI Trip)

linkAgencies :: URI
linkAgencies = safeLink (Proxy @RestAPI)
               (Proxy @("api" :> "agency" :> Get '[JSON] [Agency]))

linkRoutes :: URI
linkRoutes = safeLink (Proxy @RestAPI)
             (Proxy @("api" :> "route" :> Get '[JSON] [Route]))

linkServices :: URI
linkServices = safeLink (Proxy @RestAPI)
               (Proxy @("api" :> "service" :> Get '[JSON] [Service]))

linkTripsForRoute :: Identifier Route -> URI
linkTripsForRoute = safeLink (Proxy @RestAPI)
                    (Proxy @("api" :> "route" :> RouteTripsAPI))
