-- | Page specifiers for the TellMeT application.
module TellMeT.Pages where

import           Data.Default (Default (def))
import           Data.Text    (Text)

import           TellMeT.GTFS (Route)
import           TellMeT.Util (Identifier)

-- | The set of pages in the application.
data Page = NoPage
          | RouteList
          | RoutePage (Identifier Text Route)
          deriving (Eq, Show)

instance Default Page where
  def = RouteList
