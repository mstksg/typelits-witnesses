{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- |
-- Module      : GHC.TypeLits.Compare
-- Description : Tools for proving and refining inequalities and bounds on
--               GHC TypeLits types using '<=' and '<=?'
-- Copyright   : (c) Justin Le 2016
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
--
--
-- This module provides the ability to refine given 'KnownNat' instances
-- using '<=' and '<=?' from "GHC.TypeLits", and also the ability to prove
-- inequalities and upper/lower limits.
--
-- If a library function requires @1 '<=' n@ constraint, but only
-- @'KnownNat' n@ is available:
--
-- @
-- foo :: (KnownNat n, 1 '<=' n) => 'Proxy' n -> Int
--
-- bar :: KnownNat n => Proxy n -> Int
-- bar n = case (Proxy :: Proxy 1) '%<=?' n of
--           'LE'  Refl -> foo n
--           'NLE' _    -> 0
-- @
--
-- @foo@ requires that @1 <= n@, but @bar@ has to handle all cases of @n@.
-- @%<=?@ lets you compare the 'KnownNat's in two 'Proxy's and returns
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

module GHC.TypeLits.Compare
  ( (%<=?), (:<=?)(..)
  , isLE
  , isNLE
  )
  where

import Data.Type.Equality
import Data.Proxy
import GHC.TypeLits
import Unsafe.Coerce

isLE
    :: (KnownNat m, KnownNat n)
    => Proxy m
    -> Proxy n
    -> Maybe ((m <=? n) :~: 'True)
isLE m n = case m %<=? n of
             LE  Refl -> Just Refl
             NLE _    -> Nothing

isNLE
    :: (KnownNat m, KnownNat n)
    => Proxy m
    -> Proxy n
    -> Maybe ((m <=? n) :~: 'False)
isNLE m n = case m %<=? n of
              NLE Refl -> Just Refl
              LE  _    -> Nothing

data (:<=?) :: Nat -> Nat -> * where
  LE  :: ((m <=? n) :~: 'True)  -> (m :<=? n)
  NLE :: ((m <=? n) :~: 'False) -> (m :<=? n)

(%<=?)
     :: (KnownNat m, KnownNat n)
     => Proxy m
     -> Proxy n
     -> (m :<=? n)
m %<=? n | natVal m <= natVal n = LE  (unsafeCoerce Refl)
         | otherwise            = NLE (unsafeCoerce Refl)

