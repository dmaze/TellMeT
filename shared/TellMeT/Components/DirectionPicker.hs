{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.DirectionPicker where

import           Lens.Micro         (non, (^.))
import           Miso.Html          (View, text)
import           Miso.Html.Element  (label_, option_, select_)
import           Miso.Html.Event    (onChange)
import           Miso.Html.Property (for_, id_, selected_, value_)
import           Miso.String        (MisoString, fromMisoString, ms)
import           Text.Read          (readMaybe)

-- | Displays the direction picker; that is, a label and a select
-- control.  The caller is responsible for wrapping this in an input
-- group or other layout as required.
viewPickDirection
  :: Maybe Int                 -- ^ the currently selected direction (if any)
  -> [(Maybe Int, MisoString)] -- ^ the list of choices
  -> (Maybe Int -> action)     -- ^ create an action when picked
  -> [View action]             -- ^ a set of elements that can be embedded
viewPickDirection selected directions pickDirection =
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
