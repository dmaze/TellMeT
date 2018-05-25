{-# LANGUAGE OverloadedStrings #-}

-- | Primitives for the Bootstrap component library.
module TellMeT.Bootstrap where

import Data.Monoid ((<>))
import Miso.Html (Attribute, View, text)
import Miso.Html.Element (a_, button_, div_, nav_, span_)
import Miso.Html.Property (textProp, class_, href_, type_)
import Miso.String (MisoString, ms)
import Network.URI (URI)

-- Getting Started

container_ :: [View a] -> View a
container_ = div_ [class_ "container"]

-- Layout

row_ :: [View a] -> View a
row_ = div_ [class_ "row"]

col_sm_ :: [View a] -> View a
col_sm_ = div_ [class_ "col-sm"]

-- Alerts

-- | Produce an alert wrapping some content.  The context is one of
-- "primary" (blue), "secondary" (gray), "success" (green), "danger"
-- (red), "warning" (yellow), "info" (teal), "light" (white), or
-- "dark" (gray).  (Which should probably be an enum.)
alert_ :: MisoString -> [View a] -> View a
alert_ context = div_ [ class_ ("alert alert-" <> context)
                      , textProp "role" "alert"
                      ]

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

-- Navbar

-- | Produce a wrapping navbar.  All navbar content goes within this.
navbar_ :: [View a] -> View a
navbar_ = nav_ [class_ "navbar navbar-expand-lg navbar-light bg-light"]

-- | Produce the "brand" primary link in the navbar.
navbar_brand_ :: URI -> [View a] -> View a
navbar_brand_ uri = a_ [class_ "navbar-brand", href_ (ms $ show $ uri)]

-- Not actually Bootstrap

-- | Produce a Font Awesome icon.  This strongly assumes the "solid"
-- style (since the "regular" and "light" styles cost money).
--
-- The actual HTML element is just produced by string manipulation,
-- so if you need it to spin, you can include helpers like "fa-spin"
-- inside the string.
fa_ :: MisoString -> View a
fa_ icon = span_ [class_ $ "fas fa-" <> icon] []
