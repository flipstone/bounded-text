{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, SomeNat (..), Symbol, UnconsSymbol, natVal, someNatVal, symbolVal, type (+), type (<=))
import qualified Language.Haskell.TH.Quote as Quote
import qualified Language.Haskell.TH.Syntax as TH

newtype BoundedText (minLen :: Nat) (maxLen :: Nat) = BoundedText T.Text
  deriving (Eq, Ord, Show, TH.Lift)

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

{- | Safely converts a 'BoundedText' into a more lenient 'BoundedText'
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

{- | Convert a type level Symbol to a 'BoundedText'.

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
boundedTextFromSymbol = BoundedText (T.pack $ symbolVal (Proxy @symbol))

type family Length (s :: Symbol) :: Nat where
  Length s = ComputeLength (UnconsSymbol s)

type family ComputeLength (r :: Maybe (Char, Symbol)) :: Nat where
  ComputeLength Nothing = 0
  ComputeLength (Just '(c, ts)) = 1 + Length ts

{- | QuasiQuoter for creating a 'BoundedText'.

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
  Quote.QuasiQuoter
    { Quote.quoteExp = \str ->
        let
          lenVal = fromIntegral (length str)
        in
          case someNatVal lenVal of
            Just (SomeNat (_ :: Proxy len)) ->
              case boundedTextFromText @len @len (T.pack str) of
                Left err -> fail $ describeBoundedTextError err
                Right bounded -> do
                  boundedExp <- TH.lift bounded
                  let
                    litLen = TH.LitT (TH.NumTyLit lenVal)
                    -- The signature here is necessary for correctness by enforcing the generated value has the correct bounds
                    typeSig = TH.AppT (TH.AppT (TH.ConT ''BoundedText) litLen) litLen
                  pure $ TH.AppE (TH.VarE 'boundedTextSafeCoerce) (TH.SigE boundedExp typeSig)
            Nothing -> fail "QuasiQuote could not get the length of the string to construct the bounded text"
    , Quote.quotePat = const $ fail "QuasiQuote patterns not supported for bounded text"
    , Quote.quoteType = const $ fail "QuasiQuote types not supported for bounded text"
    , Quote.quoteDec = const $ fail "QuasiQuote Declarations not supported for bounded text"
    }
