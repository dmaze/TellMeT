{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Display a page for a given route.
module TellMeT.Components.RoutePage where

import           Data.Default                       (def)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import qualified Data.Set                           as Set
import           Lens.Micro                         (at, ix, non, (^.), (^..))
import           Miso.Html                          (View, text)
import           Miso.Html.Element                  (div_, form_, h1_, li_,
                                                     span_, ul_)
import           Miso.Html.Property                 (class_)
import           Miso.String                        (MisoString, ms)

#ifdef __GHCJS__
import           Control.Monad.Writer.Class         (tell)
import           Lens.Micro                         (each, (^?))
import           Lens.Micro.Mtl                     (use, (<%=))
import           Miso.Types                         (Transition)
#endif

import           TellMeT.Bootstrap                  (alert_, fa_)
import           TellMeT.Components.DirectionPicker (PickDirection,
                                                     PickedDirection,
                                                     pickedDirection,
                                                     viewPickDirection)
import           TellMeT.Components.FeedFetcher     (FeedFetchAction (fetchTripsForRoute),
                                                     FeedFetcher (tripsForRouteFetcher),
                                                     HasFeed (theFeed),
                                                     ifFetchedTripsForRoute,
                                                     viewAFetch)
import           TellMeT.Components.Fetcher         (Fetcher (Fetched, Unfetched))
import           TellMeT.Components.Pages           (PageAction)
import           TellMeT.Components.RouteBadge      (viewRouteBadge)
import           TellMeT.Components.ServicePicker   (PickService, PickedService,
                                                     pickedService,
                                                     serviceSummary,
                                                     viewPickService)
import           TellMeT.GTFS                       (Route, Service, Trip,
                                                     routes, services,
                                                     showStopTimeTime,
                                                     stopTimeDepartureTime,
                                                     tripBikesAllowed,
                                                     tripDirectionId,
                                                     tripHeadsign,
                                                     tripServiceId,
                                                     tripShortName,
                                                     tripStopTimes,
                                                     tripWheelchairAccessible)
import           TellMeT.Pages                      (Page)
import           TellMeT.Util                       (Identifier, MapOf)

-- | Display the page for a given route.  Note that the identifier will
-- come out of the URL, not out of the model state.
viewRoutePage :: (FeedFetcher model, HasFeed model, PickedService model,
                 PickedDirection model, PickDirection action,
                 PageAction Page action, PickService action)
              => Identifier Route -> model -> View action
viewRoutePage routeId model =
  let feed = model ^. theFeed
      theServices = feed ^. services
      aService = model ^. pickedService
      aDirection = model ^. pickedDirection
      fetch = model ^. tripsForRouteFetcher . at routeId . non Unfetched
  in case feed ^. routes . at routeId of
    Nothing    -> viewNoRoutePage
    Just route -> viewARoutePage route aService aDirection theServices fetch

-- | Display an error page for an invalid route ID.
viewNoRoutePage :: View action
viewNoRoutePage =
  alert_ "danger" [fa_ "frown", text $ "Uh oh, I don't know that route."]

viewARoutePage :: (PageAction Page action, PickService action,
                  PickDirection action)
               => Route
               -> Maybe (Identifier Service)
               -> Maybe Int
               -> MapOf Service
               -> Fetcher [Trip]
               -> View action
viewARoutePage route aService aDirection theServices fetch = div_ []
  [ h1_ [] [ viewRouteBadge route ]
  , viewRouteTrips aService aDirection theServices fetch
  ]

tripTitle :: Trip -> MisoString
tripTitle trip = ms $
  if tripShortName trip /= ""
  then tripShortName trip <> ": " <> tripHeadsign trip
  else tripHeadsign trip

viewRouteTrips :: (PickService action, PickDirection action)
               => Maybe (Identifier Service)
               -> Maybe Int
               -> MapOf Service
               -> Fetcher [Trip]
               -> View action
viewRouteTrips aService aDirection theServices (Fetched trips) =
  let someServices = do sid <- visibleServices trips
                        theServices ^.. ix sid
      someDirections = visibleDirections trips aService
      picker = viewPickService aService someServices <>
               viewPickDirection aDirection someDirections
      someTrips = visibleTrips aService aDirection trips
      lis = viewRouteTrip theServices <$> someTrips
  in div_ []
     [ form_ [ class_ "form-inline" ] picker
     , ul_ [ class_ "list-unstyled" ] lis
     ]
viewRouteTrips _ _ _ fetch = viewAFetch "Fetching trips" fetch

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

-- | Get a list of direction IDs and descriptions from a list
-- of trips and a selected service.
visibleDirections :: [Trip] -> Maybe (Identifier Service) -> [(Maybe Int, MisoString)]
visibleDirections trips aService =
  let trips' = filterByService aService trips
      tripDir t = Map.singleton
                  (tripDirectionId t)
                  (Set.singleton $ tripHeadsign t)
      directionMap = unMapList $ foldMap (MapList . tripDir) trips'
  in Map.toList $ oxfordComma <$> directionMap

-- | Map of keys to lists, where the Monoid instance appends lists.
newtype MapList k a = MapList { unMapList :: (Map k a) }

instance (Ord k, Monoid a) => Monoid (MapList k a) where
  mempty = MapList mempty
  mappend (MapList l) (MapList r) = MapList $ Map.unionWith (<>) l r

data OxfordComma a = None | One a | Two a a | Many [a]

instance Functor OxfordComma where
  fmap _ None      = None
  fmap f (One x)   = One (f x)
  fmap f (Two x y) = Two (f x) (f y)
  fmap f (Many xs) = Many (fmap f xs)

oxfordToList :: OxfordComma a -> [a]
oxfordToList None      = []
oxfordToList (One x)   = [x]
oxfordToList (Two x y) = [x, y]
oxfordToList (Many xs) = xs

instance Monoid (OxfordComma a) where
  mempty = None
  mappend None y          = y
  mappend x None          = x
  mappend (One x) (One y) = Two x y
  mappend x y             = Many ((oxfordToList x) <> (oxfordToList y))

oxfordToString :: OxfordComma MisoString -> MisoString
oxfordToString None = "(nothing)"
oxfordToString (One x) = x
oxfordToString (Two x y) = x <> " or " <> y
oxfordToString (Many l) = more l
  where more []     = ", or a mistake" -- shouldn't happen
        more [x]    = ", or " <> x
        more (x:xs) = x <> ", " <> (more xs)

-- | Convert a list of strings by properly inserting commas.
oxfordComma :: (Foldable t) => t MisoString -> MisoString
oxfordComma = oxfordToString . foldMap One

-- | Filter a list of trips to those having a given service ID
-- (if one is known).
filterByService :: Maybe (Identifier Service) -> [Trip] -> [Trip]
filterByService Nothing    = id
filterByService (Just sid) = filter ((== sid) . tripServiceId)

-- | Filter a list of trips to those having a given direction ID
-- (where @Nothing@ is a valid direction).
filterByDirection :: Maybe Int -> [Trip] -> [Trip]
filterByDirection dir = filter ((== dir) . tripDirectionId)

-- | Get a list of trips that will actually be displayed.
visibleTrips :: Maybe (Identifier Service) -> Maybe Int -> [Trip] -> [Trip]
visibleTrips aService aDirection = filterByDirection aDirection .
                                   filterByService aService

#ifdef __GHCJS__
onRoutePage :: (FeedFetcher model, FeedFetchAction action)
            => Identifier Route
            -> Transition action model ()
onRoutePage routeId = do
  fetcher <- use $ tripsForRouteFetcher . at routeId
  case maybe Unfetched id fetcher of
    Unfetched -> tell [ \d -> d $ fetchTripsForRoute routeId ]
    _         -> return ()

updateRoutePage :: (PickedService model, PickedDirection model,
                    FeedFetchAction action)
                => action -> Transition action model ()
updateRoutePage a = do
  ifFetchedTripsForRoute a $ \_ fetch ->
    case fetch of
      Fetched trips -> do aService <- updatePickedService trips
                          _ <- updatePickedDirection trips aService
                          return ()
      _ -> return ()

-- | Given a list of newly visible trips, update the chosen
-- service if necessary.
updatePickedService :: (PickedService model)
                    => [Trip]
                    -> Transition action model (Maybe (Identifier Service))
updatePickedService trips =
  let someServices = visibleServices trips
      anyService = someServices ^? each
      pickValidService Nothing = anyService
      pickValidService (Just sid) =
        if sid `elem` someServices
        then Just sid
        else anyService
  in pickedService <%= pickValidService

-- | Given a list of newly visible trips and a chosen service,
-- update the chosen direction if necessary.
updatePickedDirection :: (PickedDirection model)
                      => [Trip]
                      -> Maybe (Identifier Service)
                      -> Transition action model (Maybe Int)
updatePickedDirection trips aService =
  let trips' = filterByService aService trips
      directions = foldMap (Set.singleton . tripDirectionId) trips'
      pickValidDirection dir =
        if dir `Set.member` directions
        then dir
        else if Set.null directions
             then Nothing
             else Set.findMin directions
  in pickedDirection <%= pickValidDirection
#endif
