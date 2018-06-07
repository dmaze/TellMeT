{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Display a page for a given route.
module TellMeT.Components.RoutePage where

import           Data.Default                   (def)
import           Data.Monoid                    ((<>))
import           Lens.Micro                     (at, non, (^.))
import           Miso.Html                      (View, text)
import           Miso.Html.Element              (div_, h1_, li_, span_, ul_)
import           Miso.Html.Property             (class_)
import           Miso.String                    (MisoString, ms)

#ifdef __GHCJS__
import           Control.Monad.Writer.Class     (tell)
import           Lens.Micro.Mtl                 (use)
import           Miso.Types                     (Transition)
#endif

import           TellMeT.Bootstrap              (alert_, fa_)
import           TellMeT.Components.FeedFetcher (FeedFetchAction (fetchTripsForRoute),
                                                 FeedFetcher (tripsForRouteFetcher),
                                                 HasFeed (theFeed), viewAFetch)
import           TellMeT.Components.Fetcher     (Fetcher (Fetched, Unfetched))
import           TellMeT.Components.Pages       (PageAction)
import           TellMeT.Components.RouteBadge  (viewRouteBadge)
import           TellMeT.GTFS                   (Calendar, CalendarDate, ExceptionType (Added, NoException, Removed),
                                                 Route, Service,
                                                 Trip (tripBikesAllowed, tripHeadsign, tripShortName, tripWheelchairAccessible),
                                                 calendarDateDate,
                                                 calendarDateExceptionType,
                                                 calendarFriday, calendarMonday,
                                                 calendarSaturday,
                                                 calendarSunday,
                                                 calendarThursday,
                                                 calendarTuesday,
                                                 calendarWednesday, routes,
                                                 serviceCalendar, serviceDates,
                                                 services, tripServiceId)
import           TellMeT.Pages                  (Page)
import           TellMeT.Util                   (Identifier, MapOf)

-- | Display the page for a given route.  Note that the identifier will
-- come out of the URL, not out of the model state.
viewRoutePage :: (FeedFetcher model, HasFeed model, PageAction Page action)
              => Identifier Route -> model -> View action
viewRoutePage routeId model =
  let feed = model ^. theFeed
      theServices = feed ^. services
      fetch = model ^. tripsForRouteFetcher . at routeId . non Unfetched
  in case feed ^. routes . at routeId of
    Nothing    -> viewNoRoutePage
    Just route -> viewARoutePage route theServices fetch

-- | Display an error page for an invalid route ID.
viewNoRoutePage :: View action
viewNoRoutePage =
  alert_ "danger" [fa_ "frown", text $ "Uh oh, I don't know that route."]

viewARoutePage :: (PageAction Page action)
  => Route -> MapOf Service -> Fetcher [Trip] -> View action
viewARoutePage route theServices fetch = div_ []
  [ h1_ [] [ viewRouteBadge route ]
  , viewRouteTrips theServices fetch
  ]

tripTitle :: Trip -> MisoString
tripTitle trip = ms $
  if tripShortName trip /= ""
  then tripShortName trip <> ": " <> tripHeadsign trip
  else tripHeadsign trip

serviceSummary :: Service -> MisoString
serviceSummary s = mconcat $
                   [calendarSummary (serviceCalendar s)] <>
                   (calendarDateSummary <$> serviceDates s)

calendarSummary :: Maybe Calendar -> MisoString
calendarSummary Nothing = "Only"
calendarSummary (Just cal) = mconcat $
  [ if calendarMonday cal then "M" else ""
  , if calendarTuesday cal then "T" else ""
  , if calendarWednesday cal then "W" else ""
  , if calendarThursday cal then "R" else ""
  , if calendarFriday cal then "F" else ""
  , if calendarSaturday cal then "Sa" else ""
  , if calendarSunday cal then "Su" else ""
  ]

calendarDateSummary :: CalendarDate -> MisoString
calendarDateSummary cd =
  " " <> sym (calendarDateExceptionType cd) <> ms (calendarDateDate cd)
  where sym NoException = "?"
        sym Added       = "+"
        sym Removed     = "-"

viewRouteTrips :: MapOf Service -> Fetcher [Trip] -> View action
viewRouteTrips theServices (Fetched trips) =
  ul_ [ class_ "list-unstyled" ] (viewRouteTrip theServices <$> trips)
viewRouteTrips _ fetch = viewAFetch "Fetching trips" fetch

viewRouteTrip :: MapOf Service -> Trip -> View action
viewRouteTrip theServices trip =
  let service = theServices ^. at (tripServiceId trip) . non def
      title = tripTitle trip <> " (" <> serviceSummary service <> ")"
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

#ifdef __GHCJS__
onRoutePage :: (FeedFetcher model, FeedFetchAction action)
            => Identifier Route
            -> Transition action model ()
onRoutePage routeId = do
  fetcher <- use $ tripsForRouteFetcher . at routeId
  case maybe Unfetched id fetcher of
    Unfetched -> tell [ \d -> d $ fetchTripsForRoute routeId ]
    _         -> return ()
#endif
