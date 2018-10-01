{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

-- |
-- Module      : GHC.TypeLits.Witnesses
-- Description : Instance witnesses for various arithmetic operations on
--               GHC TypeLits.
-- Copyright   : (c) Justin Le 2016
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
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
--


module GHC.TypeLits.Witnesses
  {-# DEPRECATED "Use singletons package instead" #-} (
  -- * Singletons
  -- $singletons
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

import           Data.Constraint
import           Data.Proxy
import           Data.Reflection
import           GHC.TypeLits
import           Unsafe.Coerce

-- $singletons
--
-- All of the functionality in this module can be subsumed by the
-- /singletons/ package, by utilizing:
--
--   * "Data.Singletons"
--   * "Data.Singletons.TypeLits"
--   * "Data.Singletons.Prelude.Num"
--
-- This module is left in this package as an alternative for those who
-- might, for some reason, not want to add a /singletons/ dependency to
-- their project.  However, if you do much at the type level, using the
-- /singletons/ library is much preferred, as it provides a unifed
-- interface for all of the functionality here, generalized to other kinds
-- besides 'Nat'.
--
-- For all functions in this module, a /singletons/ equivalent is included
-- for help migrating.
--
-- In general:
--
--
--   * The /singletons/ type @'Sing' n@ (or its equivalent, @'SNat' n@)
--     subsumes both @'Proxy' n@ and @'Dict' ('KnownNat' n)@.  You can
--     replace both @'Proxy' n@ and @'Dict' ('KnownNat' n)@ with @'SNat' n@
--     to move to singletons style.
--
--   * 'dictNatVal' and 'natVal' are both just 'fromSing'.
--
--   * Replace '%+', '%-', and '%*' with their /singletons/
--     equivalents, '%:+', '%:-', and '%:*' from
--     "Data.Singletons.Prelude.Num".  Note that the current version of
--     /singletons/ does not have an equivalent for '%^'.
--
--   * Use 'withKnownNat' from /singletons/ (or just pattern match on
--     'SNat') to get a 'KnownNat' instance from a @'SNat' n@, the same way
--     you'd get one from a 'Dict'.
--
--   * The high-level combinator 'withNatOp' can simply be replaced with
--     applying your singleton functions ('%+' etc.) to 'SNat' values, and
--     pattern matching on the result, or using 'withKnownNat' on the result.
--

-- | Create a 'Dict' witness for @'KnownNat' n@.
--
-- Not necessary with /singletons/, as @'SNat' n@ stands in for both
-- @'Proxy' n@ and @'Dict' ('KnownNat' n)@.
natDict :: KnownNat n => p n -> Dict (KnownNat n)
natDict _ = Dict

-- | Get the 'Integer' from the 'KnownNat' instance witnessed by the
-- 'Dict'.
--
-- With /singletons/, this is 'fromSing', which takes an @'SNat' n@ and
-- returns an 'Integer'.
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
--
-- With /singletons/, this is '%:+' from "Data.Singletons.Prelude.Num".
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
--
-- With /singletons/, this is '%:-' from "Data.Singletons.Prelude.Num".
(%-) :: forall n m. Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat (n - m))
Dict %- Dict = mapDict entailSub (Dict :: Dict (KnownNat n, KnownNat m))

-- | Given witnesses for @'KnownNat' n@ and @'KnownNat' m@, generates
-- a witness for @'KnownNat' (n * m)@.
--
-- Follows proper association and fixity for usage with other similar
-- operators.
--
-- With /singletons/, this is '%:*' from "Data.Singletons.Prelude.Num".
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
-- With /singletons/, @'withNatOp' f x y@ is @'withKnownNat' (f x y)@.
--
-- So, instead of
--
-- @
-- 'withNatOp' ('%+') ('Proxy' :: 'Proxy' n) ('Proxy' :: 'Proxy' 1) $
--     'natVal' ('Proxy' :: 'Proxy' (n + 1))
-- @
--
-- You can just use
--
-- @
-- 'withKnownNat' ('SNat' @n) ('SNat @1) $
--     'natVal' ('Proxy' :: 'Proxy' (n + 1))
-- @
--
-- 'natVal' can of course be replaced with 'fromSing'.
--
withNatOp
    :: (KnownNat n, KnownNat m)
    => (Dict (KnownNat n) -> Dict (KnownNat m) -> Dict (KnownNat q))
    -> p n
    -> p m
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

