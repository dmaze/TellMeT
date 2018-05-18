{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lens.Micro.Mtl ((.=))
import Miso (App(..), Effect, Transition, defaultEvents,  miso,
             fromTransition)

import TellMeT.Action (Action(ChangeURI, FetchFeed))
import TellMeT.Components.FeedFetcher (updateFeedFetch)
import TellMeT.Model (Model, initialModel, siteUri)
import TellMeT.Routes (viewModel)

updateModel_ :: Action -> Transition Action Model ()
updateModel_ (ChangeURI uri) = siteUri .= uri
updateModel_ _ = return ()

updateModel :: Action -> Model -> Effect Action Model
updateModel action = fromTransition $ do
  updateFeedFetch action
  updateModel_ action

main :: IO ()
main = miso $ \uri -> App { model = initialModel uri
                         , update = updateModel
                         , view = viewModel
                         , subs = []
                         , events = defaultEvents
                         , initialAction = FetchFeed
                         , mountPoint = Nothing
                         }
