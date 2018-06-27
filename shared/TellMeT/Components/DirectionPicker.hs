{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.DirectionPicker where

import           Lens.Micro         (non, (^.))
import           Miso.Html          (View, text)
import           Miso.Html.Element  (label_, option_, select_)
import           Miso.Html.Event    (onChange)
import           Miso.Html.Property (for_, id_, selected_, value_)
import           Miso.String        (MisoString, fromMisoString, ms)
import           Text.Read          (readMaybe)

-- | Models that have a single direction chosen.  Trips, in
-- particular, have directions.  GTFS specifies that the "direction"
-- column is optional but that if it is present it should be 0 or 1,
-- so hopefully this is consistent across routes (and if not then
-- Nothing is a sensible choise probably).
class PickedDirection model where
  -- | Lens on the model to the currently-picked direction.
  pickedDirection :: (Functor t)
                => (Maybe Int -> t (Maybe Int))
                -> model
                -> t model

-- | Actions that can choose a direction.
class PickDirection action where
  -- | Create an action to pick a direction.
  pickDirection :: Maybe Int -> action

-- | Displays the direction picker; that is, a label and a select
-- control.  The caller is responsible for wrapping this in an input
-- group or other layout as required.
viewPickDirection
  :: (PickDirection action)
  => Maybe Int                 -- ^ the currently selected direction (if any)
  -> [(Maybe Int, MisoString)] -- ^ the list of choices
  -> [View action]             -- ^ a set of elements that can be embedded
viewPickDirection selected directions =
  [ label_ [for_ "pick-direction-select"] [text "Direction: "]
  , select_ [ id_ "pick-direction-select"
            , onChange (\v -> pickDirection (readMaybe $ fromMisoString v))
            ] (makeOption <$> directions)
  ]
  where makeOption (dir, desc) =
          option_
          [ value_ $ (ms <$> show <$> dir) ^. non ""
          , selected_ (selected == dir)
          ]
          [ text desc ]
