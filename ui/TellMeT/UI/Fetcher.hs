{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Fetch data from REST endpoints.
module TellMeT.UI.Fetcher(fetch) where

import           Control.Exception               (catch)
import           Data.Default                    (Default (def))
import           Data.Monoid                     ((<>))
import           JavaScript.JSON.Types.Instances (FromJSON, fromJSON)
import           JavaScript.JSON.Types.Internal  (Result (Error, Success))
import           JavaScript.Web.XMLHttpRequest   (Method (GET),
                                                  Request (Request),
                                                  RequestData (NoData),
                                                  Response (contents, status),
                                                  XHRError (XHRAborted, XHRError),
                                                  xhr)
import           Miso.String                     (MisoString, ms)
import           Network.URI                     (URI)
import           TellMeT.Model.Fetcher           (Fetcher (FetchFailed, Fetched))

-- |Fetch an arbitrary URI, expecting it to return JSON data.
-- Produces @Fetched@ on success, and @FetchFailed@ if the network
-- connection failed, it produced a non-200 HTTP response, JSON
-- parsing failed, or JSON unmarshaling failed.
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
          Nothing -> FetchFailed "empty response"
          Just v  -> fromValue v
        code -> FetchFailed $ "server returned status " <> (ms $ show code)
  in catch (fromResponse <$> xhr request) fromError

{- The essay:

XMLHttpRequest can operate in a couple of modes.  The "obvious"
Haskell approach is to ask it to return text, package it into a
ByteString, and then use the normal Aeson parsing sequence, but this
is sloooooow.

If we accept that we're in JavaScript land, then the "obvious"
JavaScript approach is to ask XMLHttpRequest to do the JSON parsing
for us and return a JavaScript object.  ghcjs-base has its own magic
shadow copy of the Data.Aeson tree (and its own FromJS interface).
The other piece of magic here is that @xhr@ passes on a response
format to XMLHttpRequest dependent on the result type, so if we ask it
for a ghcjs-base JSON Value then ghcjs will ask JavaScript land to do
the JSON parsing for us.

The implementation above accepts being tied to this interface and does
the straightforward thing, once you pick apart the relevant bits of
source code (Google failed at finding any actual documentation).

In principle Miso provides some helpers for this, but it's oriented
around the standard Aeson objects (right in principle, big and slow in
practice), starts from text (we can ask XHR to start from parsed
JSON), and shoves errors into exceptions on the IO monad (yuck).

I think the fetch function here is more or less optimal, but it was a
lot of digging to find it.
-}
