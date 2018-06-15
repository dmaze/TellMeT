{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.Fetcher where

import           Data.Default                    (Default (def))
import           Miso.String                     (MisoString)

#ifdef __GHCJS__
import           Control.Exception               (catch)
import           Data.Monoid                     ((<>))
import           JavaScript.JSON.Types.Instances (FromJSON, fromJSON)
import           JavaScript.JSON.Types.Internal  (Result (Error, Success))
import           JavaScript.Web.XMLHttpRequest   (Method (GET),
                                                  Request (Request),
                                                  RequestData (NoData),
                                                  Response (contents, status),
                                                  XHRError (XHRAborted, XHRError),
                                                  xhr)
import           Miso.String                     (ms)
import           Network.URI                     (URI)
#endif

data Fetcher a = Unfetched
               | Fetching
               | FetchFailed MisoString
               | Fetched a
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

instance Default (Fetcher a) where
  def = Unfetched

#ifdef __GHCJS__
fetch :: (FromJSON a) => URI -> IO (Fetcher a)
fetch uri =
  let request = Request GET (ms $ "/" <> show uri) Nothing [] False NoData
      fromError :: XHRError -> IO (Fetcher a)
      fromError (XHRError t) = return $ FetchFailed (ms t)
      fromError XHRAborted   = return $ FetchFailed "request aborted"
      fromValue v = case fromJSON v of
        Error s   -> FetchFailed (ms s)
        Success x -> Fetched x
      fromResponse response = case status response of
        200 -> case contents response of
          Nothing -> return $ FetchFailed "empty response"
          Just v  -> return $ fromValue v
        code -> return $ FetchFailed $ "server returned status " <> (ms $ show code)
  in catch (xhr request >>= fromResponse) fromError

{- The essay:

XMLHttpRequest can operate in a couple of modes.  The "obvious"
Haskell approach is to ask it to return text, package it into a
ByteString, and then use the normal Aeson parsing sequence, but this
is sloooooow.

If we accept that we're in JavaScript land, then the "obvious"
JavaScript approach is to ask XMLHttpRequest to do the JSON parsing
for us and return a JavaScript object.  ghcjs-base has its own magic
shadow copy of the Data.Aeson tree (and its own FromJS interface).

If you continue diving into the guts of this,
JavaScript.JSON.Types.Internal has two types.  SomeValue is just a
wrapper around a JSVal and becomes a flag to xhr to request JSON
parsing.  SomeValue' is a Haskell model of a JSON object.  So we need
to request a (SomeValue Immutable) (aliased as Value), then convert
that to an Aeson Value, then hand-run the Aeson fromJSON.

Miso provides a couple parts of this.  jsvalToValue (which we call)
converts a JSVal to an Aeson Value, with enough FFI to have to run in
the IO monad.  It also has a higher-level parse function which calls
JS JSON.parse on a JSString, then calls jsvalToValue, then calls
fromJSON, carefully shoving all failures into exceptions on the IO
monad...which really isn't what we want.

I think the fetch function here is more or less optimal, but it was a
lot of digging to find it.
-}
#endif
