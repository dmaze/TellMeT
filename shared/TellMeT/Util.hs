{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Miscellaneous utilities.
module TellMeT.Util where

import           Data.Map.Strict    (Map, insert)
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

#ifdef __GHCJS__
-- la la la orphan instances
instance FromField JSString where
  parseField = return . textToJSString . decodeLatin1

instance FromHttpApiData JSString where
  parseUrlPiece = Right . textToJSString

instance ToHttpApiData JSString where
  toUrlPiece = textFromJSString
#endif
