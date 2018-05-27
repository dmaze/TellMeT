{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | Core client page routing.
module TellMeT.Routes where

import           Data.Default                 (def)
import           Lens.Micro                   ((^.))
import           Miso                         (View, text)
import           Network.URI                  (URI)

import           TellMeT.Action               (Action)
import           TellMeT.Components.Chrome    (viewChrome)
import           TellMeT.Components.Pages     (currentPage, goToPageLink)
import           TellMeT.Components.RouteList (viewRouteList)
import           TellMeT.Components.RoutePage (viewRoutePage)
import           TellMeT.Model                (Model)
import           TellMeT.Pages                (Page (NoPage, RouteList, RoutePage))

viewModel :: Model -> View Action
viewModel model =
  viewChrome home $
  case model ^. currentPage of
    NoPage        -> view404
    RouteList     -> viewRouteList model
    RoutePage rid -> viewRoutePage rid model
  where (_, home) = goToPageLink def :: (Action, URI)

view404 :: View a
view404 = text "not found"
