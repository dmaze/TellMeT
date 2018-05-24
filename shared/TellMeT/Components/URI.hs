{-# LANGUAGE CPP #-}

-- | Handle the browser URI.
module TellMeT.Components.URI where

import Lens.Micro (Lens')
import Network.URI (URI)

#ifdef __GHCJS__
import Lens.Micro.Mtl ((.=))
import Miso.Subscription.History (pushURI)
import Miso.Types (Transition, scheduleSub)
#endif

-- | Models that are aware of the current site URI.
class SiteURI model where
  -- | Access the current site URI.
  siteURI :: Lens' model URI

-- | Actions that can affect the current site URI.
class URIAction action where
  -- | An action that causes the site URI to change.  This does not
  -- cause an immediate change in the model; instead, it changes the
  -- browser URL, and waits for a handleURIChange action to be
  -- produced.
  changeURI :: URI -> action
  
  -- | Run a monadic action if the requested action is a change-URI
  -- action.
  ifChangeURI :: (Monad m) => action -> (URI -> m ()) -> m ()
  
  -- | An action taken in response to the site URI changing.  This
  -- action probably wants to be fired in response to an
  -- application-global subscription using Miso's 'uriSub' function.
  handleURIChange :: URI -> action

  -- | Run a monadic action if the requested action is to handle a
  -- changed site URI.
  ifHandleURIChange :: (Monad m) => action -> (URI -> m ()) -> m ()

#ifdef __GHCJS__
-- | Update the model and invoke other side effects in response to
-- URI-related actions.
updateURI :: (SiteURI model, URIAction action)
          => action
          -> Transition action model ()
updateURI action = do
  ifChangeURI action $ \uri ->
    scheduleSub $ \_ -> pushURI uri
  ifHandleURIChange action $ \uri ->
    siteURI .= uri
#endif
