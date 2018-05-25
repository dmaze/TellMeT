{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module TellMeT.Components.FeedFetcher where

import Data.Monoid ((<>))
import Lens.Micro ((^.), Lens')
import Lens.Micro.GHC ()
import Miso.Html (View, div_, p_, text)
import Miso.String (MisoString, ms)

import TellMeT.Bootstrap (fa_)
import TellMeT.Components.Fetcher
  ( Fetcher (Unfetched, Fetching, FetchFailed, Fetched)
#ifdef __GHCJS__
  , fetch
#endif
  )
import TellMeT.GTFS
  ( Agency, Feed, Route
#ifdef __GHCJS__
  , agencies, routes, putMap
#endif
  )

#ifdef __GHCJS__
import Control.Monad.Writer.Class (tell)
import Data.Default (def)
import Lens.Micro.Mtl ((.=), use)
import Miso.Types (Transition)
import TellMeT.REST (linkAgencies, linkRoutes)
#endif

class HasFeed model where
  theFeed :: Lens' model Feed

class FeedFetcher model where
  fetchAgencies :: Lens' model (Fetcher [Agency])
  fetchRoutes :: Lens' model (Fetcher [Route])

class FeedFetchAction action where
  fetchFeed :: action
  ifFetchFeed :: (Monad m) => action -> m () -> m ()
  fetchedAgencies :: Fetcher [Agency] -> action
  ifFetchedAgencies :: (Monad m)
                    => action -> (Fetcher [Agency] -> m ()) -> m ()
  fetchedRoutes :: Fetcher [Route] -> action
  ifFetchedRoutes :: (Monad m)
                  => action -> (Fetcher [Route] -> m ()) -> m ()

-- | If we have the base feed already, run some other view function;
-- otherwise ignore the view function and fetch the feed.
viewOrFetch :: (FeedFetcher model) => model -> View action -> View action
viewOrFetch m v = if haveFeed m then v else viewFeedFetch m

viewAFetch :: (Show obj) => MisoString -> Fetcher obj -> View action
viewAFetch title Unfetched = p_ []
  [ fa_ "circle"
  , text title
  ]
viewAFetch title Fetching = p_ []
  [ fa_ "circle-notch spin"
  , text title
  ]
viewAFetch title (FetchFailed msg) = p_ []
  [ fa_ "times-circle"
  , text (title <> ": " <> msg)
  ]
viewAFetch title (Fetched obj) = p_ []
  [ fa_ "check-circle"
  , text (title <> ": " <> (ms $ show $ obj))
  ]

viewFeedFetch :: (FeedFetcher model) => model -> View action
viewFeedFetch model = div_ []
  [ viewAFetch "Agencies" $ model ^. fetchAgencies
  , viewAFetch "Routes" $ model ^. fetchRoutes
  ]

haveFeed :: (FeedFetcher model) => model -> Bool
haveFeed model = do
  case (model ^. fetchAgencies, model ^. fetchRoutes) of
    (Fetched _, Fetched _) -> True
    _ -> False
  
#ifdef __GHCJS__
updateFeedFetch :: (HasFeed model, FeedFetcher model, FeedFetchAction action)
                => action
                -> Transition action model ()
updateFeedFetch a = do
  ifFetchFeed a $ do
    fetchAgencies .= Fetching
    fetchRoutes .= Fetching
    tell [ \dispatch -> fetch linkAgencies >>= dispatch . fetchedAgencies
         , \dispatch -> fetch linkRoutes >>= dispatch . fetchedRoutes
         ]
    return ()
  ifFetchedAgencies a $ \as -> do
    fetchAgencies .= as
    buildFeed
  ifFetchedRoutes a $ \rs -> do
    fetchRoutes .= rs
    buildFeed
  return ()

buildFeed :: (HasFeed model, FeedFetcher model) => Transition action model ()
buildFeed = do
  fAgencies <- use fetchAgencies
  fRoutes <- use fetchRoutes
  case (fAgencies, fRoutes) of
    (Fetched theAgencies, Fetched theRoutes) ->
      theFeed .= buildFeedFrom theAgencies theRoutes
    _ -> return ()

buildFeedFrom :: [Agency] -> [Route] -> Feed
buildFeedFrom theAgencies theRoutes =
  foldr (.) id (putMap agencies <$> theAgencies) $
  foldr (.) id (putMap routes <$> theRoutes) $
  def
#endif
