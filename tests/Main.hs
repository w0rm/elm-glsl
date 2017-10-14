module Main (main) where

import qualified Test.HUnit as TH
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFPH

import qualified Tests
import qualified NewTests

main :: IO ()
main = TF.defaultMain $ concatMap (TFPH.hUnitTestToTests . TH.TestList)
    [ Tests.parsingTests
    , NewTests.parsingTests
    ]
