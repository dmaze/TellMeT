{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Miscellaneous utilities.
module TellMeT.Util where

import           Data.Map.Strict (Map, insert)

-- | Things that have unique identifiers.
class Identified a where
  -- | The type of identifiers of this object.
  data Identifier a

  -- | The identifier for this object.
  identifier :: a -> Identifier a

-- | Shorthand for a map of objects by their identifier keys.
type MapOf a = Map (Identifier a) a

-- | Add a uniquely-identified object to a map on its primary key.
addToMap :: (Identified a, Ord (Identifier a)) => a -> MapOf a -> MapOf a
addToMap obj = insert (identifier obj) obj
