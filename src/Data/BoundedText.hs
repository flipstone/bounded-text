{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Data.BoundedText
  ( BoundedText
  , BoundedTextError (..)
  , describeBoundedTextError
  , boundedTextFromText
  , boundedTextToText
  , boundedTextMaxLength
  , boundedTextMinLength
  , boundedTextFromLiteral
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Constraint.Symbol (Length)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, natVal, symbolVal, type (<=))

newtype BoundedText (minLen :: Nat) (maxLen :: Nat) = BoundedText T.Text
  deriving (Eq, Ord, Show)

instance DeepSeq.NFData (BoundedText minLen maxLen) where
  rnf (BoundedText t) = DeepSeq.rnf t

data BoundedTextError
  = TextLengthBelowMinimum Integer
  | TextLengthAboveMaximum Integer
  deriving (Eq, Show)

describeBoundedTextError :: BoundedTextError -> String
describeBoundedTextError err =
  case err of
    TextLengthBelowMinimum n -> "Text length below minimum: " <> show n
    TextLengthAboveMaximum n -> "Text length above maximum: " <> show n

type role BoundedText nominal nominal

boundedTextFromText ::
  forall minLen maxLen.
  (KnownNat minLen, KnownNat maxLen) =>
  T.Text ->
  Either BoundedTextError (BoundedText minLen maxLen)
boundedTextFromText str =
  let
    minVal = natVal (Proxy :: Proxy minLen)
    maxVal = natVal (Proxy :: Proxy maxLen)
    len = toInteger (T.length str)
  in
    case (len >= minVal, len <= maxVal) of
      (True, True) -> Right $ BoundedText str
      (False, _) -> Left $ TextLengthBelowMinimum minVal
      (_, False) -> Left $ TextLengthAboveMaximum maxVal

boundedTextToText :: BoundedText minLen maxMax -> T.Text
boundedTextToText (BoundedText txt) = txt

boundedTextMinLength ::
  forall proxy minLen maxLen.
  KnownNat minLen =>
  proxy (BoundedText minLen maxLen) ->
  Int
boundedTextMinLength _proxy =
  fromInteger $ natVal (Proxy :: Proxy minLen)

boundedTextMaxLength ::
  forall proxy minLen maxLen.
  KnownNat maxLen =>
  proxy (BoundedText minLen maxLen) ->
  Int
boundedTextMaxLength _proxy =
  fromInteger $ natVal (Proxy :: Proxy maxLen)

-- | Convert a type level Symbol to a BoundedText.
--
-- You can call this as 'boundedTextFromLiteral @"hello" Proxy' and you don't
-- have to handle the failure, as you would with 'boundedTextFromText'.
-- @since 0.1.2.0
boundedTextFromLiteral ::
  forall symbol min max.
    ( KnownSymbol symbol
    , min <= Length symbol
    , Length symbol <= max
    ) =>
  Proxy symbol ->
  BoundedText min max
boundedTextFromLiteral proxy = BoundedText (T.pack $ symbolVal proxy)
