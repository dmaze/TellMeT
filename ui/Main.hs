{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso                               (App (..), Effect,
                                                     defaultEvents,
                                                     fromTransition, miso)
import           Miso.Subscription.History          (uriSub)

import           TellMeT.Action                     (Action (FetchFeed),
                                                     updatePage)
import           TellMeT.Components.DirectionPicker (updatePickDirection)
import           TellMeT.Components.RoutePage       (updateRoutePage)
import           TellMeT.Components.ServicePicker   (updatePickService)
import           TellMeT.Components.URI             (handleURIChange, updateURI)
import           TellMeT.Model                      (Model, initialModel)
import           TellMeT.Routes                     (viewModel)
import           TellMeT.UI.Update                  (updateFeedFetch)

updateModel :: Action -> Model -> Effect Action Model
updateModel action = fromTransition $ do
  updateFeedFetch action
  updateURI action
  updatePage action
  updateRoutePage action
  updatePickService action
  updatePickDirection action

main :: IO ()
main = miso $ \uri -> App { model = initialModel uri
                         , update = updateModel
                         , view = viewModel
                         , subs = [uriSub handleURIChange]
                         , events = defaultEvents
                         , initialAction = FetchFeed
                         , mountPoint = Nothing
                         }
