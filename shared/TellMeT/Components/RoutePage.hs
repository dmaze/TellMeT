{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Display a page for a given route.
module TellMeT.Components.RoutePage where

import           Control.Monad.Writer.Class     (tell)
import           Data.Monoid                    ((<>))
import           Lens.Micro                     (at, (^.))
import           Miso.Html                      (View, text)
import           Miso.Html.Element              (div_, h1_, li_, span_, ul_)
import           Miso.Html.Property             (class_)
import           Miso.String                    (MisoString, ms)

#ifdef __GHCJS__
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
import           TellMeT.GTFS                   (Route, Trip (tripBikesAllowed, tripHeadsign, tripShortName, tripWheelchairAccessible),
                                                 routes)
import           TellMeT.Pages                  (Page)
import           TellMeT.Util                   (Identifier)

-- | Display the page for a given route.  Note that the identifier will
-- come out of the URL, not out of the model state.
viewRoutePage :: (FeedFetcher model, HasFeed model, PageAction Page action)
              => Identifier Route -> model -> View action
viewRoutePage routeId model = case model ^. theFeed . routes . at routeId of
  Nothing    -> viewNoRoutePage
  Just route -> let fetch = maybe Unfetched id
                           (model ^. tripsForRouteFetcher . at routeId)
               in viewARoutePage route fetch

-- | Display an error page for an invalid route ID.
viewNoRoutePage :: View action
viewNoRoutePage =
  alert_ "danger" [fa_ "frown", text $ "Uh oh, I don't know that route."]

viewARoutePage :: (PageAction Page action) => Route -> Fetcher [Trip] -> View action
viewARoutePage route fetch = div_ []
  [ h1_ [] [ viewRouteBadge route ]
  , viewRouteTrips fetch
  ]

viewRouteTrips :: Fetcher [Trip] -> View action
viewRouteTrips (Fetched trips) =
  ul_ [ class_ "list-unstyled" ] $
  (\trip -> li_ []
           [ text $ ms $ tripHeadsign trip
           , viewOptionalFeature "wheelchair" (tripWheelchairAccessible trip)
           , viewOptionalFeature "bicycle" (tripBikesAllowed trip)
           ]) <$> trips
viewRouteTrips fetch = viewAFetch "Fetching trips" fetch

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
