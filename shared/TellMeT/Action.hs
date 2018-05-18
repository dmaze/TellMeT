module TellMeT.Action where

import Data.Default (Default (def))
import Network.URI (URI)

import TellMeT.Components.FeedFetcher
  ( FeedFetchAction ( fetchFeed, ifFetchFeed
                    , fetchedAgencies, ifFetchedAgencies
                    , fetchedRoutes, ifFetchedRoutes
                    )
  )
import TellMeT.Components.Fetcher (Fetcher)
import TellMeT.GTFS (Agency, Route)

data Action = NoOp
            | ChangeURI URI
            | FetchFeed
            | FetchedAgencies (Fetcher [Agency])
            | FetchedRoutes (Fetcher [Route])
            deriving (Show, Eq)

instance Default Action where
  def = NoOp

instance FeedFetchAction Action where
  fetchFeed = FetchFeed
  ifFetchFeed FetchFeed a = a
  ifFetchFeed _ _ = return ()
  fetchedAgencies = FetchedAgencies
  ifFetchedAgencies (FetchedAgencies ff) a = a ff
  ifFetchedAgencies _ _ = return ()
  fetchedRoutes = FetchedRoutes
  ifFetchedRoutes (FetchedRoutes ff) a = a ff
  ifFetchedRoutes _ _ = return ()
