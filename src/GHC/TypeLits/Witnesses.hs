{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

module GHC.TypeLits.Witnesses (
    natDict
  , dictNatVal
  , (%+)
  , (%-)
  , (%*)
  , (%^)
  , withNatOp
  , entailAdd
  , entailSub
  , entailMul
  , entailExp
  ) where

import Data.Constraint
import GHC.TypeLits
import Data.Proxy
import Data.Reflection
import Unsafe.Coerce

natDict :: KnownNat n => Proxy n -> Dict (KnownNat n)
natDict _ = Dict

dictNatVal :: forall n. Dict (KnownNat n) -> Integer
dictNatVal Dict = natVal (Proxy :: Proxy n)

infixl 6 %+
(%+) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n + m))
Dict %+ Dict = mapDict entailAdd (Dict :: Dict (KnownNat n, KnownNat m))

infixl 6 %-
(%-) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n - m))
Dict %- Dict = mapDict entailSub (Dict :: Dict (KnownNat n, KnownNat m))

infixl 7 %*
(%*) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n * m))
Dict %* Dict = mapDict entailMul (Dict :: Dict (KnownNat n, KnownNat m))

infixr 8 %^
(%^) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n ^ m))
Dict %^ Dict = mapDict entailExp (Dict :: Dict (KnownNat n, KnownNat m))

withNatOp :: (KnownNat n, KnownNat m)
          => (Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat q))
          -> Proxy n
          -> Proxy m
          -> (KnownNat q => r)
          -> r
withNatOp f x y r = case natDict x `f` natDict y of
                      Dict -> r

entailAdd :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n + m)
entailAdd = Sub $
  reifyNat (natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

entailSub :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n - m)
entailSub = Sub $
  reifyNat (natVal (Proxy :: Proxy n) - natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

entailMul :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n * m)
entailMul = Sub $
  reifyNat (natVal (Proxy :: Proxy n) * natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

entailExp :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n ^ m)
entailExp = Sub $
  reifyNat (natVal (Proxy :: Proxy n) ^ natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

