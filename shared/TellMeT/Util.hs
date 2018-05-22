{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Miscellaneous utilities.
module TellMeT.Util where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map, insert)

-- | A unique identifier for some type.
newtype Identifier k a = Identifier { unIdentifier :: k }
                       deriving (Eq, Show, Ord, FromJSON, ToJSON)

-- | Things that have unique identifiers.
class Identified k a | a -> k where
  -- | The identifier for this object.
  identifier :: a -> Identifier k a

-- | Shorthand for a map of objects by their identifier keys.
type MapOf k a = Map (Identifier k a) a

-- | Add a uniquely-identified object to a map on its primary key.
addToMap :: (Ord k, Identified k a) => a -> MapOf k a -> MapOf k a
addToMap obj = insert (identifier obj) obj
