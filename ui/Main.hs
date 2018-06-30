{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Miso                      (App (..), defaultEvents, miso)
import           Miso.Subscription.History (uriSub)

import           TellMeT.Action            (Action (FetchFeed, HandleURIChange))
import           TellMeT.Model             (initialModel)
import           TellMeT.Routes            (viewModel)
import           TellMeT.UI.Update         (updateModel)

main :: IO ()
main = miso $ \uri -> App { model = initialModel uri
                         , update = updateModel
                         , view = viewModel
                         , subs = [uriSub HandleURIChange]
                         , events = defaultEvents
                         , initialAction = FetchFeed
                         , mountPoint = Nothing
                         }
