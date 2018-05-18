{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Bootstrap where

import Data.Monoid ((<>))
import Miso.Html (Attribute, View, text)
import Miso.Html.Element (button_, div_, span_)
import Miso.Html.Property (textProp, class_, type_)
import Miso.String (MisoString)

-- Getting Started

container_ :: [View a] -> View a
container_ = div_ [class_ "container"]

-- Layout

row_ :: [View a] -> View a
row_ = div_ [class_ "row"]

col_sm_ :: [View a] -> View a
col_sm_ = div_ [class_ "col-sm"]

-- Buttons

btn_ :: [Attribute a] -> [View a] -> View a
btn_ attrs = button_ ([type_ "button", class_ "btn"] <> attrs)

-- Button group

btn_group_ :: [View a] -> View a
btn_group_ = div_ [class_ "btn-group", textProp "role" "group"]

-- Forms

form_group_ :: [View a] -> View a
form_group_ = div_ [class_ "form-group"]

-- Input group

input_group_ :: [View a] -> [View a] -> [View a] -> View a
input_group_ before body after = div_
  [class_ "input-group"]
  ([div_ [class_ "input-group-prepend"] before] <>
   body <>
   [div_ [class_ "input-group-append"] after])

input_group_text_ :: MisoString -> View a
input_group_text_ t = span_ [class_ "input-group-text"] [text t]
