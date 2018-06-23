-- |Record the state of fetching something.
module TellMeT.Model.Fetcher (Fetcher (..)) where

import           Data.Default (Default (def))
import           Miso.String  (MisoString)

-- |Record the state of fetching something.  This in effect a simple
-- state machine.
data Fetcher a
  = Unfetched              -- ^ Haven't started the fetch.
  | Fetching               -- ^ Fetching in progress.
  | FetchFailed MisoString -- ^ Fetching failed.
  | Fetched a              -- ^ Fetching complete.
  deriving (Eq, Show)

instance Functor Fetcher where
  fmap _ Unfetched       = Unfetched
  fmap _ Fetching        = Fetching
  fmap _ (FetchFailed t) = FetchFailed t
  fmap f (Fetched a)     = Fetched (f a)

instance Applicative Fetcher where
  pure = Fetched
  Unfetched <*> _ = Unfetched
  Fetching <*> _ = Fetching
  FetchFailed t <*> _ = FetchFailed t
  _ <*> Unfetched = Unfetched
  _ <*> Fetching = Fetching
  _ <*> FetchFailed t = FetchFailed t
  (Fetched f) <*> (Fetched x) = Fetched (f x)

instance Monad Fetcher where
  return = Fetched
  Unfetched >>= _ = Unfetched
  Fetching >>= _ = Fetching
  FetchFailed t >>= _ = FetchFailed t
  (Fetched x) >>= f = f x

instance Default (Fetcher a) where
  def = Unfetched

