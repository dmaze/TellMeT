{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- | Display the top-level list of routes.
module TellMeT.Components.RouteList where

import           Data.List                      (sortOn)
import           Data.Monoid                    ((<>))
import           Lens.Micro                     (each, (^..))
import           Lens.Micro.GHC                 ()
import           Miso.Html                      (View, text)
import           Miso.Html.Element              (li_, ul_)
import           Miso.Html.Property             (class_)
import           Miso.String                    (ms)

import           TellMeT.Components.FeedFetcher (viewOrFetch)
import           TellMeT.Components.Pages       (PageAction)
import           TellMeT.Components.RouteBadge  (viewRouteBadge)
import           TellMeT.GTFS                   (Route, routeDesc,
                                                 routeLongName, routeShortName,
                                                 routeSortOrder, routes)
import           TellMeT.Model.Class            (FeedFetcher, HasFeed, theFeed)
import           TellMeT.Pages                  (Page)

viewOneRoute :: (PageAction Page action) => Route -> View action
viewOneRoute route = li_ [] [viewRouteBadge route, text $ ms $ desc]
  where short = routeShortName route
        long = routeLongName route
        moreName = if short /= "" && long /= "" then long else ""
        theDesc = routeDesc route
        sep = if moreName /= "" && theDesc /= "" then ": " else ""
        desc = " " <> moreName <> sep <> theDesc

viewAllRoutes :: (PageAction Page action) => [Route] -> View action
viewAllRoutes theRoutes =
  ul_ [class_ "list-unstyled"] $
  viewOneRoute <$> sortOn routeSortOrder theRoutes

viewRouteList :: (FeedFetcher model, HasFeed model, PageAction Page action)
              => model -> View action
viewRouteList model = viewOrFetch model $
                      viewAllRoutes (model ^.. theFeed . routes . each)
