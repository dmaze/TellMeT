{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lens.Micro.Mtl ((.=))
import Miso (App(..), Effect, Transition, defaultEvents,  miso,
             fromTransition)
import Miso.Subscription.History (uriSub)

import TellMeT.Action (Action(ChangeURI, FetchFeed), updatePage)
import TellMeT.Components.FeedFetcher (updateFeedFetch)
import TellMeT.Components.URI (handleURIChange, siteURI, updateURI)
import TellMeT.Model (Model, initialModel)
import TellMeT.Routes (viewModel)

updateModel :: Action -> Model -> Effect Action Model
updateModel action = fromTransition $ do
  updateFeedFetch action
  updateURI action
  updatePage action

main :: IO ()
main = miso $ \uri -> App { model = initialModel uri
                         , update = updateModel
                         , view = viewModel
                         , subs = [uriSub handleURIChange]
                         , events = defaultEvents
                         , initialAction = FetchFeed
                         , mountPoint = Nothing
                         }
