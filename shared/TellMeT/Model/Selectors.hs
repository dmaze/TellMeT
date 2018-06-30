-- | Pick interesting bits of data out of a model.
module TellMeT.Model.Selectors where

import           Lens.Micro            ((^.))
import           TellMeT.Model.Class   (FeedFetcher, fetchAgencies, fetchRoutes,
                                        fetchServices)
import           TellMeT.Model.Fetcher (Fetcher (Fetched))

-- |Determine whether we have fetched all of the parts of the feed
-- that get fetched at startup time.
haveFeed :: (FeedFetcher model) => model -> Bool
haveFeed model = do
  case (model ^. fetchAgencies, model ^. fetchRoutes,
        model ^. fetchServices) of
    (Fetched _, Fetched _, Fetched _) -> True
    _                                 -> False
