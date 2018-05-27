{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- | URIs and Servant handlers for pages in the application.
--
-- This file will probably get split up into very generic page
-- machinery and application-specific pages/routes.
module TellMeT.Components.Pages where

import Data.Monoid ((<>))
import Miso.Event.Decoder (emptyDecoder)
import Miso.Html (Attribute, View)
import Miso.Html.Element (a_)
import Miso.Html.Event (Options (preventDefault, stopPropagation),
                        defaultOptions, onWithOptions)
import Miso.Html.Property (href_)
import Miso.String (ms)
import Network.URI (URI)

-- | Models that know what page we're on.
class OnPage page model where
  -- | Lens accessing the current page.
  currentPage :: (Functor t) => (page -> t page) -> model -> t model

-- | Actions that can act on the current page.
--
-- Components that provide links between pages should fire 'goToPage'
-- when needed.  Components that provide top-level pages should handle
-- 'ifNowOnPage' in their update function, and if they are on the
-- correct page, fire off additional actions needed to fetch state or
-- otherwise asynchronously set up the page.
--
-- There should be one top-level page router that translates 'ifGoToPage'
-- to invoking the browser history API, and that translates top-level
-- page URI changes into 'nowOnPage' actions.
class PageAction page action | action -> page where
  -- | Create an action to go to a specified page.
  goToPage :: page -> action

  -- | Create an action to go to a specified page, and also return the
  -- target of a link that goes there.
  goToPageLink :: page -> (action, URI)

  -- | Invoke a monadic action if an action is go-to-page type.
  ifGoToPage :: (Monad m) => action -> (page -> m ()) -> m ()

  -- | Create an action when we have arrived on a page.
  nowOnPage :: page -> action

  -- | Invoke a monadic action if an action is now-on-page type.
  ifNowOnPage :: (Monad m) => action -> (page -> m ()) -> m ()

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

