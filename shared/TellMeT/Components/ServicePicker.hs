{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.ServicePicker where

import           Data.Monoid        ((<>))
import           Data.Time.Calendar (showGregorian)
import           Lens.Micro.Mtl     (assign)
import           Miso.Html          (View, text)
import           Miso.Html.Element  (label_, option_, select_)
import           Miso.Html.Event    (onChange)
import           Miso.Html.Property (for_, id_, selected_, value_)
import           Miso.String        (MisoString, fromMisoString, ms)

#ifdef __GHCJS__
import           Miso.Types         (Transition)
#endif

import           TellMeT.GTFS       (Calendar, CalendarDate,
                                     CalendarDay (CalendarDay),
                                     ExceptionType (Added, NoException, Removed),
                                     Identifier (ServiceIdentifier), Service,
                                     calendarDateDate,
                                     calendarDateExceptionType, calendarEndDate,
                                     calendarFriday, calendarMonday,
                                     calendarSaturday, calendarStartDate,
                                     calendarSunday, calendarThursday,
                                     calendarTuesday, calendarWednesday,
                                     serviceCalendar, serviceDates, serviceId)

-- | Models that have a single service chosen.
class PickedService model where
  -- | Lens on the model to the currently-picked service.
  pickedService :: (Functor t)
                => (Maybe (Identifier Service) ->
                    t (Maybe( Identifier Service)))
                -> model
                -> t model

-- | Actions that can reflect a chosen service.
class PickService action where
  -- | Create an action to pick a single service.
  pickService :: Maybe (Identifier Service) -> action
  -- | Run a monadic action if the current action is a pick-service
  -- action.
  ifPickService :: (Monad m)
                => action
                -> (Maybe (Identifier Service) -> m ())
                -> m ()

-- | Displays the service picker; that is, a label and a select
-- control.  The caller is responsible for wrapping this in an input
-- group or other layout as required.
viewPickService
  :: (PickService action)
  => Maybe (Identifier Service) -- ^ the currently selected service (if any)
  -> [Service]                  -- ^ the list of choices
  -> [View action]              -- ^ a set of elements that can be embedded
viewPickService selected services =
  [ label_ [for_ "pick-service-select"] [text "Service: "]
  , select_ [ id_ "pick-service-select"
            , onChange (\v -> pickService (Just (ServiceIdentifier (fromMisoString v))))
            ] (makeOption <$> services)
  ]
  where makeOption s = option_
                       [ value_ (ms $ rawId s)
                       , selected_ (isIt s)
                       ]
                       [ text (ms $ desc s) ]
        isIt s = case selected of
          Nothing -> False
          Just ss -> ss == serviceId s
        desc s = serviceSummary s
        rawId s = sid where ServiceIdentifier sid = serviceId s

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
  , " from ", showDay (calendarStartDate cal)
  , " to ", showDay (calendarEndDate cal)
  ]

calendarDateSummary :: CalendarDate -> MisoString
calendarDateSummary cd =
  " " <> sym (calendarDateExceptionType cd) <> showDay (calendarDateDate cd)
  where sym NoException = "?"
        sym Added       = "+"
        sym Removed     = "-"

showDay :: CalendarDay -> MisoString
showDay (CalendarDay day) = ms $ showGregorian day

#ifdef __GHCJS__
updatePickService :: (PickedService model, PickService action)
                  => action -> Transition action model ()
updatePickService a =
  ifPickService a $ assign pickedService
#endif
