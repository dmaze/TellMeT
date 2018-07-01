{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Miscellaneous utilities.
module TellMeT.Util where

import           Data.Map.Strict    (Map, insert, unionWith)
import           Data.Monoid        ((<>))
import           Miso.String        (MisoString)

#ifdef __GHCJS__
import           Data.Csv           (FromField, parseField)
import           Data.JSString      (JSString)
import           Data.JSString.Text (textFromJSString, textToJSString)
import           Data.Text.Encoding (decodeLatin1)
import           Web.HttpApiData    (FromHttpApiData, ToHttpApiData,
                                     parseUrlPiece, toUrlPiece)
#endif

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

-- | Map of keys to lists, where the Monoid instance appends lists.
newtype MapList k a = MapList { unMapList :: (Map k a) }

instance (Ord k, Monoid a) => Monoid (MapList k a) where
  mempty = MapList mempty
  mappend (MapList l) (MapList r) = MapList $ unionWith (<>) l r

-- | Convert a list of strings by properly inserting commas.
oxfordComma :: (Foldable t) => t MisoString -> MisoString
oxfordComma =
  let buildString []     = "(nothing)"
      buildString [x]    = x
      buildString [x, y] = x <> " or " <> y
      buildString xs     = buildMany xs
      buildMany []     = ", or a mistake" -- shouldn't happen
      buildMany [x]    = ", or " <> x
      buildMany (x:xs) = x <> ", " <> (buildMany xs)
  in buildString . foldMap return

#ifdef __GHCJS__
-- la la la orphan instances
instance FromField JSString where
  parseField = return . textToJSString . decodeLatin1

instance FromHttpApiData JSString where
  parseUrlPiece = Right . textToJSString

instance ToHttpApiData JSString where
  toUrlPiece = textFromJSString
#endif
