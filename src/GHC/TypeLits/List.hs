{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module GHC.TypeLits.List (
    KnownNats(..)
  , SomeNats(..)
  , NatList(..)
  , someNatsVal
  , someNatsVal'
  , reifyNats
  , KnownSymbols(..)
  , SomeSymbols(..)
  , someSymbolsVal
  , reifySymbols
  , printNats
  ) where

import Data.Proxy
import Data.Reflection
import GHC.Exts        (Constraint)
import GHC.TypeLits

data NatList :: [Nat] -> * where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns) => Proxy n -> NatList ns -> NatList (n ': ns)

infixr 5 :<#
deriving instance Show (NatList ns)

class KnownNats (ns :: [Nat]) where
    natsVal  :: p ns -> [Integer]
    natsList :: NatList ns

instance KnownNats '[] where
    natsVal  _ = []
    natsList   = ØNL

instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natsVal  _ = natVal (Proxy :: Proxy n) : natsVal (Proxy :: Proxy ns)
    natsList   = Proxy :<# natsList

data SomeNats :: * where
    SomeNats :: KnownNats ns => NatList ns -> SomeNats

someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal []     = Just (SomeNats ØNL)
someNatsVal (n:ns) = do
    SomeNat  m  <- someNatVal n
    SomeNats ms <- someNatsVal ns
    return $ SomeNats (m :<# ms)

reifyNats :: [Integer] -> (forall ns. KnownNats ns => NatList ns -> r) -> r
reifyNats []     f = f ØNL
reifyNats (n:ns) f = reifyNat n $ \m ->
                       reifyNats ns $ \ms ->
                         f (m :<# ms)

someNatsVal' :: [Integer] -> SomeNats
someNatsVal' ns = reifyNats ns SomeNats

data SymbolList :: [Symbol] -> * where
    ØSL   :: SymbolList '[]
    (:<$) :: (KnownSymbol n, KnownSymbols ns) => Proxy n -> SymbolList ns -> SymbolList (n ': ns)

infixr 5 :<$
deriving instance Show (SymbolList ns)

class KnownSymbols (ns :: [Symbol]) where
    symbolsVal  :: p ns -> [String]
    symbolsList :: SymbolList ns

instance KnownSymbols '[] where
    symbolsVal  _ = []
    symbolsList    = ØSL

instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n ': ns) where
    symbolsVal  _ = symbolVal (Proxy :: Proxy n) : symbolsVal (Proxy :: Proxy ns)
    symbolsList   = Proxy :<$ symbolsList

data SomeSymbols :: * where
    SomeSymbols :: KnownSymbols ns => SymbolList ns -> SomeSymbols

someSymbolsVal :: [String] -> SomeSymbols
someSymbolsVal []     = SomeSymbols ØSL
someSymbolsVal (n:ns) =
    case someSymbolVal n of
      SomeSymbol m ->
        case someSymbolsVal ns of
          SomeSymbols ms ->
            SomeSymbols (m :<$ ms)

reifySymbols :: [String] -> (forall ns. KnownSymbols ns => SymbolList ns -> r) -> r
reifySymbols []     f = f ØSL
reifySymbols (n:ns) f = reifySymbol n $ \m ->
                          reifySymbols ns $ \ms ->
                            f (m :<$ ms)
