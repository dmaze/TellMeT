{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

-- | URIs and Servant handlers for pages in the application.
--
-- This file will probably get split up into very generic page
-- machinery and application-specific pages/routes.
module TellMeT.Components.Pages where

import           Data.Monoid          ((<>))
import           Miso.Event.Decoder   (emptyDecoder)
import           Miso.Html            (Attribute, View)
import           Miso.Html.Element    (a_)
import           Miso.Html.Event      (Options (preventDefault, stopPropagation),
                                       defaultOptions, onWithOptions)
import           Miso.Html.Property   (href_)
import           Miso.String          (ms)

import           TellMeT.Action.Class (PageAction, goToPageLink)

-- | Generate an HTML link to a page.
a_page_ :: (PageAction page action)
        => page -> [Attribute action] -> [View action] -> View action
a_page_ page attrs =
  a_ $
  [ href_ $ ms $ "/" <> show target
  , onClick action
  ] <> attrs
  where (action, target) = goToPageLink page
        onClick a = onWithOptions (defaultOptions { preventDefault = True
                                                  , stopPropagation = True })
                    "click" emptyDecoder (const a)

