{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.Fetcher where

import Data.Default (Default (def))
import Miso.String (MisoString, ms)

#ifdef __GHCJS__
import Control.Exception (catch)
import Data.Aeson (FromJSON, eitherDecode')
import Data.ByteString.Lazy (fromStrict)
import Data.Monoid ((<>))
import Network.URI (URI)
import JavaScript.Web.XMLHttpRequest (
  Method (GET), Request (Request), RequestData (NoData),
  Response (status, contents), XHRError (XHRAborted, XHRError),
  xhrByteString)
#endif

data Fetcher a = Unfetched
               | Fetching
               | FetchFailed MisoString
               | Fetched a
                 deriving (Eq, Show)

instance Functor Fetcher where
  fmap _ Unfetched = Unfetched
  fmap _ Fetching = Fetching
  fmap _ (FetchFailed t) = FetchFailed t
  fmap f (Fetched a) = Fetched (f a)

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
  let request = Request GET (ms $ show uri) Nothing [] False NoData
      fromError (XHRError t) = return $ FetchFailed (ms t)
      fromError XHRAborted = return $ FetchFailed "request aborted"
      fromByteString bs = case eitherDecode' (fromStrict bs) of
        Left oops -> FetchFailed (ms oops)
        Right x -> Fetched x
      fromResponse response = case status response of
        200 -> case contents response of
          Nothing -> FetchFailed "empty response"
          Just body -> fromByteString body
        code -> FetchFailed $ "server returned status " <> (ms $ show code)
  in catch (fromResponse <$> xhrByteString request) fromError
#endif
