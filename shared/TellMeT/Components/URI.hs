-- | Handle the browser URI.
module TellMeT.Components.URI where

import           Network.URI (URI)

-- | Actions that can affect the current site URI.
class URIAction action where
  -- | An action that causes the site URI to change.  This does not
  -- cause an immediate change in the model; instead, it changes the
  -- browser URL, and waits for a handleURIChange action to be
  -- produced.
  changeURI :: URI -> action

  -- | An action taken in response to the site URI changing.  This
  -- action probably wants to be fired in response to an
  -- application-global subscription using Miso's 'uriSub' function.
  handleURIChange :: URI -> action
