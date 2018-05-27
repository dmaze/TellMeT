-- | Match a URL against a Servant type pattern.
--
-- There are at least two readily findable implementations of this already,
-- in the servant-matcher package (which requires servant-0.13) and in
-- Miso.Router (which only matches against a Miso View object).  The
-- pattern is slightly irritating, but straightforward, especially since
-- we use a minimal set of Servant routing options here.
module TellMeT.Matcher where

import           Data.Text   (Text, pack)
import           Network.URI (URI, pathSegments)

type Location = [Text]

matchURI :: matcher -> URI -> Maybe a
