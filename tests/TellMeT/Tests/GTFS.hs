{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module TellMeT.Tests.GTFS where

import           Data.Monoid            ((<>))
import           Data.String            (fromString)
import           Test.SmallCheck        ((==>))
import           Test.SmallCheck.Series (Serial, getPositive, newtypeCons,
                                         series)
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.SmallCheck  (testProperty)

import           TellMeT.GTFS           (StopTimeTime (StopTimeTime),
                                         parseStopTimeTime, showStopTimeTime)

testGtfs :: TestTree
testGtfs = testGroup "TellMeT.GTFS"
  [ scParseShowStopTimeTime
  , scShowParseStopTimeTime
  , scShowParseStopTimeTime1
  ]

instance (Monad m) => Serial m StopTimeTime where
  series = newtypeCons (StopTimeTime . getPositive)

scParseShowStopTimeTime :: TestTree
scParseShowStopTimeTime = testProperty "parse . show stopTimeTime" $
  \stt -> Just stt == parseStopTimeTime (showStopTimeTime stt)

scShowParseStopTimeTime :: TestTree
scShowParseStopTimeTime = testProperty "show . parse stopTimeTime" $
  \h -> (h >= 0 && h < 26) ==>
  \m -> (m >= 0 && m < 60) ==>
  \s -> (s >= 0 && s < 60) ==>
  let twoDigit :: Int -> String
      twoDigit n = if n < 10 then ('0':show n) else show n
      timestamp' = twoDigit h <> ":" <> twoDigit m <> ":" <> twoDigit s
      timestamp = fromString timestamp'
  in Just timestamp == (showStopTimeTime <$> parseStopTimeTime timestamp)

scShowParseStopTimeTime1 :: TestTree
scShowParseStopTimeTime1 = testProperty "show . parse stopTimeTime (1-digit hour)" $
  \h -> (h >= 0 && h < 10) ==>
  \m -> (m >= 0 && m < 60) ==>
  \s -> (s >= 0 && s < 60) ==>
  let twoDigit :: Int -> String
      twoDigit n = if n < 10 then ('0':show n) else show n
      timestamp = show (h :: Int) <> ":" <> twoDigit m <> ":" <> twoDigit s
  in Just (fromString ('0':timestamp)) ==
     (showStopTimeTime <$> parseStopTimeTime (fromString timestamp))
