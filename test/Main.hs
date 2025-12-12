{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main
  ( main
  ) where

import Data.Either (isRight)
import Hedgehog (Property, forAll, property, withTests, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as TastyHH

import qualified Data.BoundedText as BoundedText

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "bounded-text"
      [ TastyHH.testProperty "can build text within bounds" prop_buildsWithinBounds
      , TastyHH.testProperty "text conversion honors lower bound" prop_respectsLowerBound
      , TastyHH.testProperty "text conversion honors upper bound" prop_respectsUpperBound
      , TastyHH.testProperty "upper bound can be inspected" prop_upperBoundCanBeInspected
      , TastyHH.testProperty "lower bound can be inspected" prop_lowerBoundCanBeInspected
      , TastyHH.testProperty "upper bound will overflow" prop_overflowsUpperBound
      , TastyHH.testProperty "lower bound will overflow" prop_overflowsLowerBound
      , TastyHH.testProperty "can construct from symbol" prop_canConstructFromSymbol
      , TastyHH.testProperty "can construct from quasi-quotes" prop_canConstructFromQQ
      ]

prop_buildsWithinBounds :: Property
prop_buildsWithinBounds = property $ do
  char <- forAll $ Gen.text (Range.singleton 1) Gen.unicode

  let
    oneChar :: Either BoundedText.BoundedTextError (BoundedText.BoundedText 0 1)
    oneChar = BoundedText.boundedTextFromText char

  isRight oneChar === True

prop_respectsLowerBound :: Property
prop_respectsLowerBound = withTests 1 . property $ do
  let
    zeroChar :: Either BoundedText.BoundedTextError (BoundedText.BoundedText 1 0)
    zeroChar = BoundedText.boundedTextFromText ""

  zeroChar === Left (BoundedText.TextLengthBelowMinimum 1)

prop_respectsUpperBound :: Property
prop_respectsUpperBound = property $ do
  char <- forAll $ Gen.text (Range.singleton 2) Gen.unicode

  let
    exceedsMax :: Either BoundedText.BoundedTextError (BoundedText.BoundedText 0 1)
    exceedsMax = BoundedText.boundedTextFromText char

  exceedsMax === Left (BoundedText.TextLengthAboveMaximum 1)

prop_upperBoundCanBeInspected :: Property
prop_upperBoundCanBeInspected = withTests 1 . property $ do
  BoundedText.boundedTextMaxLength @(BoundedText.BoundedText 0 0x7fff_ffff_ffff_ffff) === maxBound

prop_lowerBoundCanBeInspected :: Property
prop_lowerBoundCanBeInspected = withTests 1 . property $ do
  BoundedText.boundedTextMinLength @(BoundedText.BoundedText 32 50) === 32

prop_overflowsUpperBound :: Property
prop_overflowsUpperBound = withTests 1 . property $ do
  -- We expect an overflow since we're casting to an Int
  -- with a constant size of bits.
  BoundedText.boundedTextMaxLength @(BoundedText.BoundedText 0 0xffff_ffff_ffff_ffff {- Overflows when cast to Int -}) === (-1)

prop_overflowsLowerBound :: Property
prop_overflowsLowerBound = withTests 1 . property $ do
  BoundedText.boundedTextMinLength @(BoundedText.BoundedText 0x8000_0000_0000_0000 0 {- Overflows when cast to Int -}) === minBound

prop_canConstructFromSymbol :: Property
prop_canConstructFromSymbol = withTests 1 . property $ do
  let
    exactText :: BoundedText.BoundedText 5 5
    exactText = BoundedText.boundedTextFromSymbol @"exact"
  BoundedText.boundedTextToText exactText === "exact"
  let
    looseText :: BoundedText.BoundedText 1 6
    looseText = BoundedText.boundedTextFromSymbol @"lt"
  BoundedText.boundedTextToText looseText === "lt"

prop_canConstructFromQQ :: Property
prop_canConstructFromQQ = withTests 1 . property $ do
  let
    exactText :: BoundedText.BoundedText 5 5
    exactText = [BoundedText.boundedTextQQ|exact|]
  BoundedText.boundedTextToText exactText === "exact"
  let
    looseText :: BoundedText.BoundedText 1 6
    looseText = [BoundedText.boundedTextQQ|lt|]
  BoundedText.boundedTextToText looseText === "lt"
