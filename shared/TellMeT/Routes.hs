{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

-- | Core client page routing.
module TellMeT.Routes where

import           Data.Default                 (def)
import           Lens.Micro                   ((^.))
import           Miso                         (View, text)

import           TellMeT.Action               (Action (PickDirection, PickService),
                                               pageLink)
import           TellMeT.Components.Chrome    (viewChrome)
import           TellMeT.Components.RouteList (viewRouteList)
import           TellMeT.Components.RoutePage (viewRoutePage)
import           TellMeT.Model                (Model)
import           TellMeT.Model.Class          (currentPage)
import           TellMeT.Pages                (Page (NoPage, RouteList, RoutePage))

viewModel :: Model -> View Action
viewModel model =
  viewChrome (pageLink def) $
  case model ^. currentPage of
    NoPage        -> view404
    RouteList     -> viewRouteList model
    RoutePage rid -> viewRoutePage rid PickService PickDirection model

view404 :: View a
view404 = text "not found"
