{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : GHC.TypeLits.Compare
-- Copyright   : (c) Justin Le 2016
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
--
--
-- This module provides the ability to refine given 'KnownNat' instances
-- using "GHC.TypeLits"'s comparison API, and also the ability to prove
-- inequalities and upper/lower limits.
--
-- If a library function requires @1 '<=' n@ constraint, but only
-- @'KnownNat' n@ is available:
--
-- @
-- foo :: (KnownNat n, 1 '<=' n) => 'Data.Proxy.Proxy' n -> Int
--
-- bar :: KnownNat n => Proxy n -> Int
-- bar n = case (Proxy :: Proxy 1) '%<=?' n of
--           'LE'  'Refl' -> foo n
--           'NLE' _    -> 0
-- @
--
-- @foo@ requires that @1 <= n@, but @bar@ has to handle all cases of @n@.
-- @%<=?@ lets you compare the 'KnownNat's in two 'Data.Proxy.Proxy's and returns
-- a @:<=?@, which has two constructors, 'LE' and 'NLE'.
--
-- If you pattern match on the result, in the 'LE' branch, the constraint
-- @1 <= n@ will be satisfied according to GHC, so @bar@ can safely call
-- @foo@, and GHC will recognize that @1 <= n@.
--
-- In the 'NLE' branch, the constraint that @1 > n@ is satisfied, so any
-- functions that require that constraint would be callable.
--
-- For convenience, 'isLE' and 'isNLE' are also offered:
--
-- @
-- bar :: KnownNat n => Proxy n -> Int
-- bar n = case 'isLE' (Proxy :: Proxy 1) n of
--           'Just' Refl -> foo n
--           'Nothing'   -> 0
-- @
--
-- Similarly, if a library function requires something involving 'CmpNat',
-- you can use 'cmpNat' and the 'SCmpNat' type:
--
-- @
-- foo1 :: (KnownNat n, 'CmpNat' 5 n ~ LT) => Proxy n -> Int
-- foo2 :: (KnownNat n, CmpNat 5 n ~ GT) => Proxy n -> Int
--
-- bar :: KnownNat n => Proxy n -> Int
-- bar n = case 'cmpNat' (Proxy :: Proxy 5) n of
--           'CLT' Refl -> foo1 n
--           'CEQ' Refl -> 0
--           'CGT' Refl -> foo2 n
-- @
--
-- You can use the 'Refl' that 'cmpNat' gives you with 'flipCmpNat' and
-- 'cmpNatLE' to "flip" the inequality or turn it into something compatible
-- with '<=?' (useful for when you have to work with libraries that mix the
-- two methods) or 'cmpNatEq' and 'eqCmpNat' to get to/from witnesses for
-- equality of the two 'Nat's.
--
-- This module is useful for helping bridge between libraries that use
-- different 'Nat'-based comparison systems in their type constraints.
module GHC.TypeLits.Compare
  ( -- * '<=' and '<=?'
    (:<=?)(..)
  , (%<=?)
    -- ** Convenience functions
  , isLE
  , isNLE
    -- * 'CmpNat'
  , SCmpNat(..)
  , GHC.TypeLits.Compare.cmpNat
    -- ** Manipulating witnesses
  , flipCmpNat
  , cmpNatEq
  , eqCmpNat
  , reflCmpNat
  , cmpNatLE
  , cmpNatGOrdering
  )
  where

import           Data.Kind
import           Data.Type.Equality
import           GHC.TypeLits ( Nat, KnownNat, CmpNat
                              , type (<=?)
                              , natVal )
import           Unsafe.Coerce
import           Data.GADT.Compare

-- | Simplified version of '%<=?': check if @m@ is less than or equal to to
-- @n@.  If it is, match on @'Just' 'Refl'@ to get GHC to believe it,
-- within the body of the pattern match.
isLE
    :: (KnownNat m, KnownNat n)
    => p m
    -> q n
    -> Maybe ((m <=? n) :~: 'True)
isLE m n = case m %<=? n of
             LE  Refl -> Just Refl
             NLE _ _  -> Nothing

-- | Simplified version of '%<=?': check if @m@ is not less than or equal
-- to to @n@.  If it is, match on @'Just' 'Refl'@ to get GHC to believe it,
-- within the body of the pattern match.
isNLE
    :: (KnownNat m, KnownNat n)
    => p m
    -> q n
    -> Maybe ((m <=? n) :~: 'False)
isNLE m n = case m %<=? n of
    NLE Refl Refl -> Just Refl
    LE  _         -> Nothing

-- | Two possible ordered relationships between two natural numbers.
data (:<=?) :: Nat -> Nat -> Type where
    LE  :: ((m <=? n) :~: 'True)  -> (m :<=? n)
    NLE :: ((m <=? n) :~: 'False) -> ((n <=? m) :~: 'True) -> (m :<=? n)

-- | Compare @m@ and @n@, classifying their relationship into some
-- constructor of ':<=?'.
(%<=?)
     :: (KnownNat m, KnownNat n)
     => p m
     -> q n
     -> (m :<=? n)
m %<=? n | natVal m <= natVal n = LE  (unsafeCoerce Refl)
         | otherwise            = NLE (unsafeCoerce Refl) (unsafeCoerce Refl)

-- | Three possible ordered relationships between two natural numbers.
data SCmpNat :: Nat -> Nat -> Type where
    CLT :: (CmpNat m n :~: 'LT) -> SCmpNat m n
    CEQ :: (CmpNat m n :~: 'EQ) -> (m :~: n) -> SCmpNat m n
    CGT :: (CmpNat m n :~: 'GT) -> SCmpNat m n

-- | Compare @m@ and @n@, classifying their relationship into some
-- constructor of 'SCmpNat'.
cmpNat
    :: (KnownNat m, KnownNat n)
    => p m
    -> q n
    -> SCmpNat m n
cmpNat m n = case compare (natVal m) (natVal n) of
               LT -> CLT (unsafeCoerce Refl)
               EQ -> CEQ (unsafeCoerce Refl) (unsafeCoerce Refl)
               GT -> CGT (unsafeCoerce Refl)

-- | Flip an inequality.
flipCmpNat :: SCmpNat m n -> SCmpNat n m
flipCmpNat = \case CLT Refl      -> CGT (unsafeCoerce Refl)
                   CEQ Refl Refl -> CEQ (unsafeCoerce Refl) Refl
                   CGT Refl      -> CLT (unsafeCoerce Refl)

-- | @'CmpNat' m n@ being 'EQ' implies that @m@ is equal to @n@.
cmpNatEq :: (CmpNat m n :~: 'EQ) -> (m :~: n)
cmpNatEq = \case Refl -> unsafeCoerce Refl

-- | A witness of equality implies that @'CmpNat' m n@ is 'Eq'.
eqCmpNat :: (m :~: n) -> (CmpNat m n :~: 'EQ)
eqCmpNat = \case Refl -> unsafeCoerce Refl

-- | Inject a witness of equality into an 'SCmpNat' at 'CEQ'.
reflCmpNat :: (m :~: n) -> SCmpNat m n
reflCmpNat r = CEQ (eqCmpNat r) r

-- | Convert to ':<=?'
cmpNatLE :: SCmpNat m n -> (m :<=? n)
cmpNatLE = \case CLT Refl      -> LE  (unsafeCoerce Refl)
                 CEQ Refl Refl -> LE  (unsafeCoerce Refl)
                 CGT Refl      -> NLE (unsafeCoerce Refl) (unsafeCoerce Refl)

-- | Convert to 'GOrdering'
--
-- @since 0.4.0.0
cmpNatGOrdering :: SCmpNat n m -> GOrdering n m
cmpNatGOrdering = \case
    CLT Refl      -> GLT
    CEQ Refl Refl -> GEQ
    CGT Refl      -> GGT

