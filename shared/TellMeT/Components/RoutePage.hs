{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Display a page for a given route.
module TellMeT.Components.RoutePage where

import           Data.Default                     (def)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import           Lens.Micro                       (at, ix, non, (^.), (^..))
import           Miso.Html                        (View, text)
import           Miso.Html.Element                (div_, form_, h1_, li_, span_,
                                                   ul_)
import           Miso.Html.Property               (class_)
import           Miso.String                      (MisoString, ms)

#ifdef __GHCJS__
import           Control.Monad.Writer.Class       (tell)
import           Lens.Micro.Mtl                   (use)
import           Miso.Types                       (Transition)
#endif

import           TellMeT.Bootstrap                (alert_, fa_)
import           TellMeT.Components.FeedFetcher   (FeedFetchAction (fetchTripsForRoute),
                                                   FeedFetcher (tripsForRouteFetcher),
                                                   HasFeed (theFeed),
                                                   ifFetchedTripsForRoute,
                                                   viewAFetch)
import           TellMeT.Components.Fetcher       (Fetcher (Fetched, Unfetched))
import           TellMeT.Components.Pages         (PageAction)
import           TellMeT.Components.RouteBadge    (viewRouteBadge)
import           TellMeT.Components.ServicePicker (PickService, PickedService,
                                                   pickService, pickedService,
                                                   serviceSummary,
                                                   viewPickService)
import           TellMeT.GTFS                     (Route, Service, Trip (tripBikesAllowed, tripHeadsign, tripShortName, tripWheelchairAccessible),
                                                   routes, services,
                                                   showStopTimeTime,
                                                   stopTimeDepartureTime,
                                                   tripServiceId, tripStopTimes)
import           TellMeT.Pages                    (Page)
import           TellMeT.Util                     (Identifier, MapOf)

-- | Display the page for a given route.  Note that the identifier will
-- come out of the URL, not out of the model state.
viewRoutePage :: (FeedFetcher model, HasFeed model, PickedService model,
                 PageAction Page action, PickService action)
              => Identifier Route -> model -> View action
viewRoutePage routeId model =
  let feed = model ^. theFeed
      theServices = feed ^. services
      aService = model ^. pickedService
      fetch = model ^. tripsForRouteFetcher . at routeId . non Unfetched
  in case feed ^. routes . at routeId of
    Nothing    -> viewNoRoutePage
    Just route -> viewARoutePage route aService theServices fetch

-- | Display an error page for an invalid route ID.
viewNoRoutePage :: View action
viewNoRoutePage =
  alert_ "danger" [fa_ "frown", text $ "Uh oh, I don't know that route."]

viewARoutePage :: (PageAction Page action, PickService action)
               => Route
               -> Maybe (Identifier Service)
               -> MapOf Service
               -> Fetcher [Trip]
               -> View action
viewARoutePage route aService theServices fetch = div_ []
  [ h1_ [] [ viewRouteBadge route ]
  , viewRouteTrips aService theServices fetch
  ]

tripTitle :: Trip -> MisoString
tripTitle trip = ms $
  if tripShortName trip /= ""
  then tripShortName trip <> ": " <> tripHeadsign trip
  else tripHeadsign trip

viewRouteTrips :: (PickService action)
               => Maybe (Identifier Service)
               -> MapOf Service
               -> Fetcher [Trip]
               -> View action
viewRouteTrips aService theServices (Fetched trips) =
  let someServices = do sid <- visibleServices trips
                        theServices ^.. ix sid
      picker = viewPickService aService someServices
      someTrips = visibleTrips aService trips
      lis = viewRouteTrip theServices <$> someTrips
  in div_ []
     [ form_ [ class_ "form-inline" ] picker
     , ul_ [ class_ "list-unstyled" ] lis
     ]
viewRouteTrips _ _ fetch = viewAFetch "Fetching trips" fetch

viewRouteTrip :: MapOf Service -> Trip -> View action
viewRouteTrip theServices trip =
  let service = theServices ^. at (tripServiceId trip) . non def
      startTime = case tripStopTimes trip of
        []     -> "(no stops)"
        (st:_) -> case stopTimeDepartureTime st of
          Nothing  -> "???"
          Just dep -> ms $ showStopTimeTime dep
      title = startTime <> " " <>
              tripTitle trip <>
              " (" <> serviceSummary service <> ")"
  in li_ []
     [ text title
     , viewOptionalFeature "wheelchair" (tripWheelchairAccessible trip)
     , viewOptionalFeature "bicycle" (tripBikesAllowed trip)
     ]

viewOptionalFeature :: MisoString -> Maybe Bool -> View action
viewOptionalFeature _ Nothing = span_ [] []
viewOptionalFeature icon (Just True) = fa_ icon
viewOptionalFeature icon (Just False) =
  span_ [ class_ "fa-stack fa-2x" ]
  [ fa_ (icon <> " fa-stack-1x")
  , fa_ "ban fa-stack-2x"
  ]

-- | Get a list of distinct service IDs from a list of trips.
visibleServices :: [Trip] -> [Identifier Service]
visibleServices = Set.toAscList . Set.fromList . fmap tripServiceId

-- | Get a list of trips that will actually be displayed.
visibleTrips :: Maybe (Identifier Service) -> [Trip] -> [Trip]
visibleTrips Nothing = id
visibleTrips (Just sid) = filter isVisible
  where isVisible trip = tripServiceId trip == sid

#ifdef __GHCJS__
onRoutePage :: (FeedFetcher model, FeedFetchAction action)
            => Identifier Route
            -> Transition action model ()
onRoutePage routeId = do
  fetcher <- use $ tripsForRouteFetcher . at routeId
  case maybe Unfetched id fetcher of
    Unfetched -> tell [ \d -> d $ fetchTripsForRoute routeId ]
    _         -> return ()

updateRoutePage :: (PickService action, FeedFetchAction action)
                => action -> Transition action model ()
updateRoutePage a = do
  ifFetchedTripsForRoute a $ \_ fetch ->
    case fetch of
      Fetched trips -> let aService = case visibleServices trips of
                            []    -> Nothing
                            (s:_) -> Just s
                      in tell [ \d -> d $ pickService aService ]
      _ -> return ()
#endif
