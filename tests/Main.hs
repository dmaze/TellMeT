module Main where

import           Test.Tasty         (defaultMain, testGroup)

import           TellMeT.Tests.GTFS (testGtfs)

main = defaultMain $ testGroup "TellMeT"
  [ testGtfs ]
