{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.BoundedText
  ( BoundedText
  , BoundedTextError (..)
  , describeBoundedTextError
  , boundedTextSafeCoerce
  , boundedTextFromText
  , boundedTextToText
  , boundedTextMaxLength
  , boundedTextMinLength
  , boundedTextFromSymbol
  , boundedTextQQ
  ) where

import qualified Control.DeepSeq as DeepSeq
import Data.Proxy (Proxy)
import qualified Data.Text as T
import GHC.Exts (proxy#)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, SomeNat (..), Symbol, UnconsSymbol, natVal', someNatVal, symbolVal', type (+), type (<=))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Lift as Lift
import qualified Language.Haskell.TH.QuasiQuoter as Quote

{- | 'BoundedText' is a newtype around 'T.Text' with
minimum and maximum (inclusive) lengths that are enforced during construction.
This provides additional type system safety when handling 'T.Text' values that need to be within a certain length.
@since 0.1.1.0
-}
newtype BoundedText (minLen :: Nat) (maxLen :: Nat) = BoundedText T.Text
  deriving (Eq, Ord, Show, Lift.Lift)

instance DeepSeq.NFData (BoundedText minLen maxLen) where
  rnf (BoundedText t) = DeepSeq.rnf t

{- | Error type determining how a 'T.Text' value failed to be converted to a 'BoundedText'.
@since 0.1.1.0
-}
data BoundedTextError
  = TextLengthBelowMinimum Integer
  | TextLengthAboveMaximum Integer
  deriving (Eq, Show)

{- | Renders a 'BoundedTextError' as a human readable string.
@since 0.1.1.0
-}
describeBoundedTextError :: BoundedTextError -> String
describeBoundedTextError err =
  case err of
    TextLengthBelowMinimum n -> "Text length below minimum: " <> show n
    TextLengthAboveMaximum n -> "Text length above maximum: " <> show n

type role BoundedText nominal nominal

{- | Converts a 'T.Text' into a 'BoundedText' with the possibility of a
'Left BoundedTextError' failure describing whether the text is too long or short.
@since 0.1.1.0
-}
boundedTextFromText ::
  forall minLen maxLen.
  (KnownNat minLen, KnownNat maxLen) =>
  T.Text ->
  Either BoundedTextError (BoundedText minLen maxLen)
boundedTextFromText str =
  let
    minVal = natVal' (proxy# @minLen)
    maxVal = natVal' (proxy# @maxLen)
    len = toInteger (T.length str)
  in
    case (len >= minVal, len <= maxVal) of
      (True, True) -> Right $ BoundedText str
      (False, _) -> Left $ TextLengthBelowMinimum minVal
      (_, False) -> Left $ TextLengthAboveMaximum maxVal

{- | Converts a 'BoundedText' into a 'T.Text'.
@since 0.1.1.0
-}
boundedTextToText :: BoundedText minLen maxMax -> T.Text
boundedTextToText (BoundedText txt) = txt

{- | Safely converts a 'BoundedText' into a more lenient 'BoundedText'.
@since 0.1.2.0
-}
boundedTextSafeCoerce ::
  forall minLen1 maxLen1 minLen2 maxLen2.
  ( minLen2 <= minLen1
  , maxLen1 <= maxLen2
  ) =>
  BoundedText minLen1 maxLen1 ->
  BoundedText minLen2 maxLen2
boundedTextSafeCoerce (BoundedText text) = BoundedText text

{- | Get the lower bound for a 'BoundedText'.

This can be called like:
@
boundedTextMinLength \@(BoundedText 5 9)
@

giving a value of 5.
@since 0.1.2.0
-}
boundedTextMinLength ::
  forall boundedText minLen maxLen.
  ( KnownNat minLen
  , boundedText ~ BoundedText minLen maxLen
  ) =>
  Int
boundedTextMinLength =
  fromInteger $ natVal' (proxy# @minLen)

{- | Get the upper bound for a 'BoundedText'.

This can be called like:
@
boundedTextMinLength \@(BoundedText 5 9)
@

giving a value of 9.
@since 0.1.2.0
-}
boundedTextMaxLength ::
  forall boundedText minLen maxLen.
  ( KnownNat maxLen
  , boundedText ~ BoundedText minLen maxLen
  ) =>
  Int
boundedTextMaxLength =
  fromInteger $ natVal' (proxy# @maxLen)

{- | Convert a type level Symbol to a 'BoundedText'.

This is the recommended and simplest method for
creating a 'BoundedText' literal at compile time from a known string value.

This can be called like:
@
boundedTextFromSymbol \@"hello"
@

using TypeApplications which allows avoiding handling errors,
compared to using 'boundedTextFromText'.
@since 0.1.2.0
-}
boundedTextFromSymbol ::
  forall symbol min max.
  ( KnownSymbol symbol
  , min <= Length symbol
  , Length symbol <= max
  ) =>
  BoundedText min max
boundedTextFromSymbol = BoundedText (T.pack $ symbolVal' (proxy# @symbol))

type family Length (s :: Symbol) :: Nat where
  Length s = ComputeLength (UnconsSymbol s)

type family ComputeLength (r :: Maybe (Char, Symbol)) :: Nat where
  ComputeLength Nothing = 0
  ComputeLength (Just '(c, ts)) = 1 + Length ts

{- | QuasiQuoter for creating a 'BoundedText'.

It is recommended to use 'boundedTextFromSymbol' instead as it is simpler to use,
though this function has the advantage of computing the 'T.pack' call at compile time.

This can be called like:
@
[boundedTextQQ|hello|]
@

using QuasiQuotes which allows avoiding handling errors,
compared to using 'boundedTextFromText'.
@since 0.1.2.0
-}
boundedTextQQ :: Quote.QuasiQuoter
boundedTextQQ =
  (Quote.namedDefaultQuasiQuoter "boundedTextQQ")
    { Quote.quoteExp = \str ->
        let
          lenVal = fromIntegral (length str)
        in
          case someNatVal lenVal of
            Just (SomeNat (_ :: Proxy len)) ->
              case boundedTextFromText @len @len (T.pack str) of
                Left err -> fail $ describeBoundedTextError err
                Right bounded -> do
                  boundedExp <- Lift.lift bounded
                  let
                    litLen = TH.LitT (TH.NumTyLit lenVal)
                    -- The signature here is necessary for correctness by enforcing the generated value has the correct bounds
                    typeSig = TH.AppT (TH.AppT (TH.ConT ''BoundedText) litLen) litLen
                  pure $ TH.AppE (TH.VarE 'boundedTextSafeCoerce) (TH.SigE boundedExp typeSig)
            Nothing -> fail "QuasiQuote could not get the length of the string to construct the bounded text"
    }
