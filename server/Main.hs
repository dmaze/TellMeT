{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad.Except                 (throwError)
import           Data.Default                         (Default (def))
import           Data.Monoid                          ((<>))
import           Data.Proxy                           (Proxy (Proxy))
import           Lens.Micro                           (Lens', at, each,
                                                       filtered, set, (^.),
                                                       (^..))
import           Lucid                                (ToHtml (toHtml, toHtmlRaw),
                                                       async_, body_, charset_,
                                                       content_, defer_,
                                                       doctypehtml_, head_,
                                                       href_, link_, meta_,
                                                       name_, rel_, renderBS,
                                                       script_, src_, title_,
                                                       with)
import           Lucid.Base                           (makeAttribute)
import           Miso                                 (ToServerRoutes, View)
import           Network.HTTP.Types                   (status404)
import           Network.URI                          (URI)
import           Network.Wai                          (Application, responseLBS)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Servant.API                          ((:<|>) (..), (:>), Raw)
import           Servant.Server                       (Handler, Server, err404,
                                                       serve)
import           Servant.Utils.StaticFiles            (serveDirectory)
import           System.Console.GetOpt                (ArgDescr (NoArg, ReqArg),
                                                       ArgOrder (Permute),
                                                       OptDescr (Option),
                                                       getOpt, usageInfo)
import           System.Environment                   (getArgs)

import           Paths_TellMeT                        (getDataDir)
import           TellMeT.Action                       (Action, ViewRoutes)
import           TellMeT.Components.Pages             (goToPageLink)
import           TellMeT.GTFS                         (Feed, Route, agencies,
                                                       routes, services, stops,
                                                       tripRouteId, trips)
import           TellMeT.Model                        (initialModel)
import           TellMeT.Pages                        (Page (RouteList, RoutePage))
import           TellMeT.REST                         (MapAPI, RestAPI,
                                                       RouteAPI, RouteTripsAPI)
import           TellMeT.Routes                       (view404, viewModel)
import           TellMeT.Server.GTFS                  (readFeed)
import           TellMeT.Util                         (Identifier, MapOf)

data Options = Options { _optHelp :: Bool
                       , _optPort :: Int
                       , _optFeed :: FilePath
                       } deriving (Eq, Show)

instance Default Options where
  def = Options { _optHelp = False
                , _optPort = 3003
                , _optFeed = "gtfs.zip"
                }

optHelp :: Lens' Options Bool
optHelp f opts = (\h -> opts { _optHelp = h }) <$> f (_optHelp opts)

optPort :: Lens' Options Int
optPort f opts = (\p -> opts { _optPort = p }) <$> f (_optPort opts)

optFeed :: Lens' Options FilePath
optFeed f opts = (\p -> opts { _optFeed = p }) <$> f (_optFeed opts)

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [ Option ['h'] ["help"] (NoArg (set optHelp True))
              "display this help message"
            , Option ['p'] ["port"] (ReqArg (set optPort . read) "PORT")
              "listen for connections on PORT"
            , Option ['g'] ["gtfs"] (ReqArg (set optFeed) "FILE")
              "read GTFS feed from FILE"
            ]

main :: IO ()
main = do
  (fs, _, errs) <- getOpt Permute optDescrs <$> getArgs
  let opts = foldr ($) def fs
  case errs of
    [] ->
      if opts ^. optHelp
      then putStrLn $ usageInfo "server" optDescrs
      else do
        feedOrErr <- readFeed (opts ^. optFeed)
        case feedOrErr of
          Left err -> putStrLn err
          Right feed -> do
            let port = opts ^. optPort
            putStrLn $ "Running on port " <> (show port)
            path <- getDataDir
            run port $ logStdout $ app path feed
    _ -> mapM_ putStrLn errs

type ServerAPI = StaticAPI :<|> RestAPI :<|> ServantRoutes :<|> Raw

app :: FilePath -> Feed -> Application
app path feed = serve (Proxy @ServerAPI) (static path
                                          :<|> restHandlers feed
                                          :<|> serverHandlers
                                          :<|> page404
                                         )

type StaticAPI = "static" :> Raw

static :: FilePath -> Server StaticAPI
static = serveDirectory

restHandlers :: Feed -> Server RestAPI
restHandlers feed = mapHandlers agencies feed :<|>
                    routeHandlers feed :<|>
                    mapHandlers services feed :<|>
                    mapHandlers stops feed :<|>
                    mapHandlers trips feed

mapHandlers :: (Ord (Identifier a))
            => Lens' Feed (MapOf a) -> Feed -> Server (MapAPI a)
mapHandlers lens feed = return (feed ^.. lens . each) :<|>
                        \a -> maybe (throwError err404) return
                             (feed ^. lens . at a)

routeHandlers :: Feed -> Server RouteAPI
routeHandlers feed = mapHandlers routes feed :<|>
                     routeTripHandlers feed

routeTripHandlers :: Feed -> Server RouteTripsAPI
routeTripHandlers feed routeId =
  -- An index on would be clever, huh.
  return $ feed ^.. trips . each . filtered (\t -> tripRouteId t == routeId)

type ServantRoutes = ToServerRoutes ViewRoutes HtmlPage Action

serverHandlers :: Server ServantRoutes
serverHandlers = routeListServer :<|> routePageServer

routeListServer :: Handler (HtmlPage (View Action))
routeListServer = pageServer RouteList

routePageServer :: Identifier Route -> Handler (HtmlPage (View Action))
routePageServer = pageServer . RoutePage

pageServer :: Page -> Handler (HtmlPage (View Action))
pageServer page = pure $ HtmlPage $ viewModel $ initialModel uri
  where (_, uri) = goToPageLink page :: (Action, URI)

page404 :: Application
page404 = \_ respond -> respond $ responseLBS
  status404 [("Content-Type", "text/html")] $
  renderBS $ toHtml $ view404

newtype HtmlPage a = HtmlPage a

instance ToHtml a => ToHtml (HtmlPage a) where
  toHtmlRaw = toHtml
  toHtml (HtmlPage x) =
    doctypehtml_ $ do
    head_ $ do
      title_ "TellMeT"
      meta_ [ charset_ "utf-8" ]
      meta_ [ name_ "viewport"
            , content_ "width=device-width, initial-scale=1, shrink-to-fit=no" ]
      link_ [ rel_ "stylesheet"
            , href_ "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
            , makeAttribute "integrity" "sha384-WskhaSGFgHYWDcbwN70/dfYBj47jz9qbsMId/iRN3ewGhXQFZCSftd1LZCfmhktB"
            , makeAttribute "crossorigin" "anonymous"
            ]
      link_ [ rel_ "stylesheet"
            , href_ "https://use.fontawesome.com/releases/v5.0.13/css/all.css"
            , makeAttribute "integrity" "sha384-DNOHZ68U8hZfKXOrtjWvjxusGo9WQnrNx2sqG0tfsghAvtVlRW3tvkXWZh58N9jp"
            , makeAttribute "crossorigin" "anonymous"
            ]
      with (script_ mempty) [ src_ "/static/all.js"
                            , async_ ""
                            , defer_ ""
                            ]
    body_ $ toHtml x
