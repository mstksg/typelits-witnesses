{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}

-- |
-- Module      : GHC.TypeLits.Witnesses
-- Description : Instance witnesses for various arithmetic operations on
--               GHC TypeLits.
-- Copyright   : (c) Justin Le 2015
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
--
--
-- This module provides witnesses for instances that result from the
-- various arithmetic operations on GHC TypeLits 'Nat' types.  In general,
-- if you have @'KnownNat' n@, GHC can't infer @'KnownNat' (n + 1)@; and if
-- you have @'KnownNat' m@, as well, GHC can't infer @'KnownNat' (n + m)@.
--
-- This can be extremely annoying when dealing with libraries and
-- applications where one regularly adds and subtracts type-level nats and
-- expects 'KnownNat' instances to follow.  For example, vector
-- concatenation of length-encoded vector types can be:
--
-- @
-- concat :: ('KnownNat' n, 'KnownNat' m) => Vector n a -> Vector m a -> Vector (n + m) a
-- @
--
-- But, now @n + m@ does not have a 'KnownNat' instance...which makes
-- operations like this extremely less useful!
--
-- At the highest level, this module can be used with 'withNatOp':
--
-- @
-- getDoubled :: forall n. 'KnownNat' n => 'Proxy' n -> 'Integer'
-- getDoubled p = 'withNatOp' ('%*') p ('Proxy' :: 'Proxy' 2) $
--     natVal ('Proxy' :: 'Proxy' (n * 2))
-- @
--
-- With the final argument of 'withNatOp', you can provide a result
-- computed in an environment where @n * 2@ is indeed an instance of
-- 'KnownNat'.
--
-- For more complex usage, you can directly manipulate witnesses and then
-- use them via pattern matching:
--
-- @
-- let pn = 'natDict' ('Proxy' :: 'Proxy' n)
--     p1 = 'natDict' ('Proxy' :: 'Proxy' 1)
--     p2 = 'natDict' ('Proxy' :: 'Proxy' 2)
-- in  case pn '%*' p2 '%+' p1 of
--       'Dict' -> 'natVal' ('Proxy' :: 'Proxy' (n * 2 + 1))
-- @
--
-- In the branch of the case statement, @n * 2 + 1@ indeed has a 'KnownNat'
-- instance.
--
-- Note that the operators have appropriate fixities to mimic value-level
-- arithmetic operations.
--
-- __WARNING__: '%-' and 'entailSub' are is implemented in a way such
-- that /negative/ 'KnownNat's are produced without any errors.  The
-- production of witnesses and entailments will hold, but be aware that any
-- functions that rely on 'KnownNat' instances to be non-negative can
-- potentially break.


module GHC.TypeLits.Witnesses (
  -- * High level wrapper
    withNatOp
  -- * Direct witnesses
  , natDict
  , dictNatVal
  -- * Witness generators
  , (%+)
  , (%-)
  , (%*)
  , (%^)
  -- * Entailments
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

-- | Create a 'Dict' witness for @'KnownNat' n@.
natDict :: KnownNat n => Proxy n -> Dict (KnownNat n)
natDict _ = Dict

-- | Get the 'Integer' from the 'KnownNat' instance witnessed by the
-- 'Dict'.
dictNatVal :: forall n. Dict (KnownNat n) -> Integer
dictNatVal Dict = natVal (Proxy :: Proxy n)

infixl 6 %+
infixl 6 %-
infixl 7 %*
infixr 8 %^

-- | Given witnesses for @'KnownNat' n@ and @'KnownNat' m@, generates
-- a witness for @'KnownNat' (n + m)@.
--
-- Follows proper association and fixity for usage with other similar
-- operators.
(%+) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n + m))
Dict %+ Dict = mapDict entailAdd (Dict :: Dict (KnownNat n, KnownNat m))

-- | Given witnesses for @'KnownNat' n@ and @'KnownNat' m@, generates
-- a witness for @'KnownNat' (n - m)@.
--
-- Note that this is implemented in a way such that /negative/ 'KnownNat's
-- are produced without any errors.
--
-- Follows proper association and fixity for usage with other similar
-- operators.
(%-) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n - m))
Dict %- Dict = mapDict entailSub (Dict :: Dict (KnownNat n, KnownNat m))

-- | Given witnesses for @'KnownNat' n@ and @'KnownNat' m@, generates
-- a witness for @'KnownNat' (n * m)@.
--
-- Follows proper association and fixity for usage with other similar
-- operators.
(%*) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n * m))
Dict %* Dict = mapDict entailMul (Dict :: Dict (KnownNat n, KnownNat m))

-- | Given witnesses for @'KnownNat' n@ and @'KnownNat' m@, generates
-- a witness for @'KnownNat' (n ^ m)@.
--
-- Follows proper association and fixity for usage with other similar
-- operators.
(%^) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n ^ m))
Dict %^ Dict = mapDict entailExp (Dict :: Dict (KnownNat n, KnownNat m))

-- | A high-level the interface of this module.  Give it one of
-- the witness-generating operators on 'KnownNat's in this module ('%+',
-- '%-', '%*', or '%^'), two 'Proxy's containing the 'KnownNat's to
-- be modified, and receive an environment where the result of the
-- operation (applied to the 'KnownNat's) has a 'KnownNat' instance.
--
-- For example, with
--
-- @
-- 'withNatOp' ('%+') ('Proxy' :: 'Proxy' n) ('Proxy' :: 'Proxy' 1) r
-- @
--
-- in @r@, @n + 1@ has a 'KnownNat' instance:
--
-- @
-- 'withNatOp' ('%+') ('Proxy' :: 'Proxy' n) ('Proxy' :: 'Proxy' 1) $
--     'natVal' ('Proxy' :: 'Proxy' (n + 1))
-- -- => will return the 'Integer' correpsonding to n + 1
-- @
--
-- Normally, if @n@ is a 'KnownNat' instance, it is not in general
-- inferrable that @n + 1@ also has a 'KnownNat' instance.  This combinator
-- makes it so.
--
-- For multiple operations on values, this can be chained:
--
-- @
-- 'withNatOp' ('%*') ('Proxy' :: 'Proxy' n) ('Proxy' :: 'Proxy' 2) $
--   'withNatOp' ('%+') ('Proxy' :: 'Proxy' (n*2)) ('Proxy' :: 'Proxy' 1) $
--     'natVal' ('Proxy' :: 'Proxy' (n * 2 + 1))
-- @
--
-- But, at this point, it's easier and simpler to just directly use the
-- operators and pattern match:
--
-- @
-- let pn = 'natDict' ('Proxy' :: 'Proxy' n)
--     p1 = 'natDict' ('Proxy' :: 'Proxy' 1)
--     p2 = 'natDict' ('Proxy' :: 'Proxy' 2)
-- in  case pn '%*' p2 '%+' p1 of
--       'Dict' -> 'natVal' ('Proxy' :: 'Proxy' (n * 2 + 1))
-- @
--
-- (Note that associativity and fixity for the witness-generating operators
-- are set to match that of normal addition and multiplication, etc.)
--
--
withNatOp
    :: (KnownNat n, KnownNat m)
    => (Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat q))
    -> Proxy n
    -> Proxy m
    -> (KnownNat q => r)
    -> r
withNatOp op x y r = case natDict x `op` natDict y of
                       Dict -> r

-- | An entailment for addition of 'KnownNat' instances.
entailAdd :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n + m)
entailAdd = Sub $
  reifyNat (natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

-- | An entailment for subtraction of 'KnownNat' instances.
--
-- Note that this is implemented in a way such that /negative/ 'KnownNat's
-- are produced without any errors.
entailSub :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n - m)
entailSub = Sub $
  reifyNat (natVal (Proxy :: Proxy n) - natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

-- | An entailment for multiplication of 'KnownNat' instances.
entailMul :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n * m)
entailMul = Sub $
  reifyNat (natVal (Proxy :: Proxy n) * natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

-- | An entailment for exponentiation of 'KnownNat' instances.
entailExp :: forall n m. (KnownNat n, KnownNat m) :- KnownNat (n ^ m)
entailExp = Sub $
  reifyNat (natVal (Proxy :: Proxy n) ^ natVal (Proxy :: Proxy m)) $ \p ->
    unsafeCoerce (natDict p)

