{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module GHC.TypeLits.List (
    KnownNats
  , SomeNats
  , natsVal
  , someNatsVal
  , someNatsVal'
  , reifyNats
  , KnownSymbols
  , SomeSymbols
  , symbolsVal
  , someSymbolsVal
  , reifySymbols
  ) where

import GHC.TypeLits
import GHC.Exts (Constraint)
import Data.Proxy
import Data.Reflection
import Data.Type.Product

type family KnownNats (ns :: [Nat]) :: Constraint where
    KnownNats '[]       = ()
    KnownNats (n ': ns) = (KnownNat n, KnownNats ns)

data SomeNats :: * where
    SomeNats :: KnownNats ns => Prod Proxy ns -> SomeNats

natsVal :: KnownNats ns => Prod proxy ns -> [Integer]
natsVal p = case p of
              Ø       -> []
              n :< ns -> natVal n : natsVal ns

someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal []     = Just (SomeNats Ø)
someNatsVal (n:ns) = do
    SomeNat  m  <- someNatVal n
    SomeNats ms <- someNatsVal ns
    return $ SomeNats (m :< ms)

someNatsVal' :: [Integer] -> SomeNats
someNatsVal' ns = reifyNats ns SomeNats

reifyNats :: [Integer] -> (forall ns. KnownNats ns => Prod Proxy ns -> r) -> r
reifyNats []     f = f Ø
reifyNats (n:ns) f = reifyNat n $ \m ->
                       reifyNats ns $ \ms ->
                         f (m :< ms)


type family KnownSymbols (ns :: [Symbol]) :: Constraint where
    KnownSymbols '[]       = ()
    KnownSymbols (n ': ns) = (KnownSymbol n, KnownSymbols ns)

data SomeSymbols :: * where
    SomeSymbols :: KnownSymbols ns => Prod Proxy ns -> SomeSymbols

symbolsVal :: KnownSymbols ns => Prod proxy ns -> [String]
symbolsVal p = case p of
                 Ø       -> []
                 n :< ns -> symbolVal n : symbolsVal ns

someSymbolsVal :: [String] -> SomeSymbols
someSymbolsVal []     = SomeSymbols Ø
someSymbolsVal (n:ns) =
    case someSymbolVal n of
      SomeSymbol m ->
        case someSymbolsVal ns of
          SomeSymbols ms ->
            SomeSymbols (m :< ms)

reifySymbols :: [String] -> (forall ns. KnownSymbols ns => Prod Proxy ns -> r) -> r
reifySymbols []     f = f Ø
reifySymbols (n:ns) f = reifySymbol n $ \m ->
                          reifySymbols ns $ \ms ->
                            f (m :< ms)

