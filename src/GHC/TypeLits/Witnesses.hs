{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE NoStarIsType              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

-- |
-- Module      : GHC.TypeLits.Witnesses
-- Copyright   : (c) Justin Le 2016
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
--
-- This module essentially provides a lightweight subset of the
-- /singletons/ library specifically for 'Nat' and 'Symbol', from
-- "GHC.TypeLits".
--
-- Its main functionality is for first-class manipulation of 'KnownNat' and
-- 'KnownSymbol' constraints.  For example, in general, if you have
-- @'KnownNat' n@, GHC can't infer @'KnownNat' (n + 1)@.  And, if you have
-- both @'KnownNat' n@ and @'KnownNat' m@, GHC can't infer @'KnownNat (n
-- + m)@.
--
-- This can be annoying when dealing with libraries and applications where
-- one regularly adds and subtracts type-level nats and expects 'KnownNat'
-- instances to follow.  For example, vector concatenation of
-- length-encoded vector types can be:
--
-- @
-- concat :: ('KnownNat' n, 'KnownNat' m) => Vector n a -> Vector m a -> Vector (n + m) a
-- @
--
-- But, now @n + m@ does not have a 'KnownNat' instance...which makes
-- operations like this much less useful.
--
-- Usually, the easiest way to get around this is to use a typechecker
-- plugin, like
-- <https://hackage.haskell.org/package/ghc-typelits-knownnat>.  However,
-- we can do this without the help of a typechecker plugin using
-- first-class values, at the cost of some increased verbosity.
--
-- We introduce @'SNat' n@, which is a term-level witness of knownnat-ness
-- that can be manipulated as a first-class value.
--
-- If we have @'KnownNat' n@, we can construct an @'SNat' n@:
--
-- @
-- 'SNat' :: KnownNat n -> SNat n
-- @
--
-- Furthermore, if we have an @'SNat' n@, we can /pattern match/ on the
-- 'SNat' constructor to get a @'KnownNat' n@ constraint:
--
-- @
-- myFunc :: SNat n -> Bool
-- myFunc SNat = ...  -- in this body, we have `KnownNat n`
-- @
--
-- So if we have @'KnownNat' n@ and @'KnownNat' m@, we can get @'KnownNat'
-- (n + m)@ by using '%+', which adds together 'SNat's:
--
-- @
-- case (SNat :: SNat n) %+ (SNat :: SNat m) of
--   SNat -> -- in this branch, we have `KnownNat (n + m)`
-- @
--
-- Note that this module converts between 'SNat' and 'Natural', and not
-- 'SNat' and 'Integer', in "GHC.TypeNats"-style.
--
-- Of course, all of this functionality is provided by the /singletons/
-- library, in "Data.Singletons.TypeLits".  This module can be useful if
-- you want a lightweight alternative without the full might of
-- /singletons/.  The main benefit of the /singletons/ library is providing
-- a unified interface for singletons of /all/ different kinds/types, and
-- not just 'Natural' and 'String'.
module GHC.TypeLits.Witnesses (
  -- * Nats
    SNat(..)
  , SomeNat(SomeNat_)
  , Natural(FromSNat)
  , fromSNat
  , withKnownNat
  , withSomeNat
  , toSomeNat
  -- ** Operations
  , (%+)
  , (%-)
  , minusSNat
  , minusSNat_
  , (%*)
  , (%^)
  -- ** Compare
  , (%<=?)
  , sCmpNat
  -- ** Unsafe
  , unsafeLiftNatOp1
  , unsafeLiftNatOp2
  -- * Symbols
  , SSymbol(..)
  , SomeSymbol(SomeSymbol_)
  , pattern FromSSymbol
  , fromSSymbol
  , withKnownSymbol
  , withSomeSymbol
  , toSomeSymbol
  ) where

import           Data.GADT.Compare
import           Data.GADT.Show
import           Data.Proxy
import           Data.Type.Equality
import           GHC.Natural
import           GHC.TypeLits ( KnownSymbol, SomeSymbol(..)
                              , symbolVal, someSymbolVal, sameSymbol )
import           GHC.TypeLits.Compare hiding ((%<=?))
import           GHC.TypeNats ( KnownNat, SomeNat(..), CmpNat
                              , type (+), type (-), type (*), type (^)
                              , natVal, someNatVal, sameNat )
import           Unsafe.Coerce
import qualified GHC.TypeLits.Compare        as Comp

-- | An @'SNat' n@ is a witness for @'KnownNat' n@.
--
-- This means that if you pattern match on the 'SNat' constructor, in that
-- branch you will have a @'KnownNat' n@ constraint.
--
-- @
-- myFunc :: SNat n -> Bool
-- myFunc SNat = ...  -- in this body, we have `KnownNat n`
-- @
--
-- This is essentially a singleton for 'Nat', and stands in for the
-- /singletons/ 'SNat' and 'Data.Singleton.Sing' types.
data SNat n = KnownNat n => SNat

deriving instance Eq (SNat n)
deriving instance Ord (SNat n)

instance Show (SNat n) where
    showsPrec d x@SNat = showParen (d > 10) $
      showString "SNat @" . showsPrec 11 (fromSNat x)

instance GShow SNat where
    gshowsPrec = showsPrec

instance TestEquality SNat where
    testEquality (SNat :: SNat n) (SNat :: SNat m) =
      flip fmap (sameNat (Proxy :: Proxy n) (Proxy :: Proxy m)) $ \case
        Refl -> Refl

instance GEq SNat where
    geq = testEquality

instance GCompare SNat where
    gcompare x = cmpNatGOrdering . sCmpNat x

data SomeNat__ = forall n. SomeNat__ (SNat n)

-- | A useful pattern synonym for matching on a 'SomeNat' as if it
-- contained an @'SNat' n@, and not a @'Proxy' n@ as it exists in
-- "GHC.TypeLits".
--
-- A layer of compatibility letting us use the original 'SomeNat' type in
-- a way that works well with 'SNat'.
--
-- This stands in for the /singletons/ 'Data.Singleton.SomeSing' constructor.
pattern SomeNat_ :: SNat n -> SomeNat
pattern SomeNat_ x <- ((\case SomeNat (Proxy :: Proxy n) -> SomeNat__ (SNat :: SNat n)) -> SomeNat__ x)
  where
    SomeNat_ (SNat :: SNat n) = SomeNat (Proxy :: Proxy n)
{-# COMPLETE SomeNat_ #-}

-- | A useful pattern synonym for matching on a 'Natural' as if it "were"
-- a 'SNat':
--
-- @
-- myFunc :: Natural -> Bool
-- myFunc (FromSNat x) = ...  -- x is `SNat n`, with `n` coming from the input
-- @
--
-- It can be used as a function, as well, to convert an @'SNat' n@ back
-- into the 'Natural' that it represents.
--
-- This stands in for the /singletons/ 'Data.Singleton.FromSing' pattern synonym.
pattern FromSNat :: SNat n -> Natural
pattern FromSNat x <- ((\i -> withSomeNat i SomeNat_) -> SomeNat_ x)
  where
    FromSNat = fromSNat
{-# COMPLETE FromSNat #-}

-- | Given an @'SNat' n@ and a value that would require a @'KnownNat' n@
-- instance, create that value.
--
-- This stands in for the function of the same name from
-- "Data.Singletons.TypeLits".
withKnownNat :: SNat n -> (KnownNat n => r) -> r
withKnownNat SNat x = x

-- | Promote ("reify") a 'Natural' to an @'SNat' n@, by providing
-- a continuation that would handle it in a way that is polymorphic over
-- all possible @n@.
--
-- This stands in the /singletons/ 'Data.Singleton.withSomeSing' function.
withSomeNat :: Natural -> (forall n. SNat n -> r) -> r
withSomeNat (someNatVal->SomeNat (Proxy :: Proxy n)) x = x (SNat :: SNat n)

-- | Promote ("reify") a 'Natural' to an @'SNat' n@ existentially hidden
-- inside a 'SomeNat'.  To use it, pattern match using 'SomeNat_'.
--
-- This stands in the /singletons/ 'Data.Singleton.toSomeSing' function.
toSomeNat :: Natural -> SomeNat
toSomeNat = someNatVal

-- | Convert ("reflect") an 'SNat' back into the 'Natural' it represents.
--
-- This stands in the /singletons/ 'Data.Singleton.fromSing' function.
fromSNat :: SNat n -> Natural
fromSNat x@SNat = natVal x

-- | Lift a unary operation to act on an @'SNat' n@ that returns an @'SNat'
-- m@.  The function given must properly describe the relationship between
-- @n@ and @m@.
--
-- For example:
--
-- @
-- double :: SNat n -> SNat (n * 2)
-- double = unsafeLiftNatOp1 (*2)
-- @
--
-- The correctness of the relationship is not checked, so be aware that
-- this can cause programs to break.
unsafeLiftNatOp1
    :: (Natural -> Natural)
    -> SNat n
    -> SNat m
unsafeLiftNatOp1 f x = withSomeNat (f (fromSNat x)) unsafeCoerce

-- | Lift a binary operation to act on an @'SNat' n@ and @'SNat' m@ that
-- returns an @'SNat' o@.  The function given must properly describe the
-- relationship between @n@, @m@, and @o@.
--
-- For example:
--
-- @
-- multiply :: SNat n -> SNat m -> SNat (n * m)
-- multiply = unsafeLiftNatOp2 (*)
-- @
--
-- The correctness of the relationship is not checked, so be aware that
-- this can cause programs to break.
unsafeLiftNatOp2
    :: (Natural -> Natural -> Natural)
    -> SNat n
    -> SNat m
    -> SNat o
unsafeLiftNatOp2 f x y = withSomeNat (f (fromSNat x) (fromSNat y)) unsafeCoerce

-- | Addition of 'SNat's.
--
-- This also will provide the correct 'KnownNat' instance for @'SNat' (n
-- + m)@, so can be used as a way to "add" 'KnownNat' instances.
--
-- This stands in for the function with the same name from
-- "Data.Singletons.Prelude.Num".
(%+) :: SNat n -> SNat m -> SNat (n + m)
(%+) = unsafeLiftNatOp2 (+)

-- | Subtraction of 'SNat's.  Note that this is unsafe, as will trigger
-- a run-time underflow if @m@ is bigger than @n@ even though it will always
-- succeed at compiletime.
--
-- This also will provide the correct 'KnownNat' instance for @'SNat' (n
-- - m)@, so can be used as a way to "subtract" 'KnownNat' instances.
--
-- This stands in for the function with the same name from
-- "Data.Singletons.Prelude.Num".
(%-) :: SNat n -> SNat m -> SNat (n - m)
(%-) = unsafeLiftNatOp2 (-)

-- | A safe version of '%-': it will return 'Left' if @n@ is less than @m@
-- (with a witness that it is), or else return the subtracted 'SNat' in
-- 'Right' in a way that is guarunteed to not have runtime underflow.
minusSNat
    :: SNat n
    -> SNat m
    -> Either (CmpNat n m :~: 'LT) (SNat (n - m))
minusSNat (fromSNat->x) (fromSNat->y) = case minusNaturalMaybe x y of
    Nothing -> Left (unsafeCoerce Refl)
    Just z  -> withSomeNat z (Right . unsafeCoerce)

-- | A version of 'minusSNat' that just returns a 'Maybe'.
minusSNat_ :: SNat n -> SNat m -> Maybe (SNat (n - m))
minusSNat_ x = either (const Nothing) Just . minusSNat x

-- | Multiplication of 'SNat's.
--
-- This also will provide the correct 'KnownNat' instance for @'SNat' (n
-- * m)@, so can be used as a way to "multiply" 'KnownNat' instances.
--
-- This stands in for the function with the same name from
-- "Data.Singletons.Prelude.Num".
(%*) :: SNat n -> SNat m -> SNat (n * m)
(%*) = unsafeLiftNatOp2 (*)

-- | Exponentiation of 'SNat's.
--
-- This also will provide the correct 'KnownNat' instance for @'SNat' (n
-- ^ m)@, so can be used as a way to "exponentiate" 'KnownNat' instances.
--
-- This stands in for the function with the same name from
-- "Data.Singletons.TypeLits".
(%^) :: SNat n -> SNat m -> SNat (n ^ m)
(%^) = unsafeLiftNatOp2 (^)

-- | Compare @n@ and @m@, categorizing them into one of the constructors of
-- ':<=?'.
(%<=?) :: SNat n -> SNat m -> n :<=? m
x@SNat %<=? y@SNat = x Comp.%<=? y

-- | Compare @n@ and @m@, categorizing them into one of the constructors of
-- 'SCmpNat'.
sCmpNat :: SNat n -> SNat m -> SCmpNat n m
sCmpNat x@SNat y@SNat = GHC.TypeLits.Compare.cmpNat x y

-- | An @'SSymbol' n@ is a witness for @'KnownSymbol' n@.
--
-- This means that if you pattern match on the 'SSymbol' constructor, in that
-- branch you will have a @'KnownSymbol' n@ constraint.
--
-- @
-- myFunc :: SSymbol n -> Bool
-- myFunc SSymbol = ...  -- in this body, we have `KnownSymbol n`
-- @
--
-- This is essentially a singleton for 'Symbol', and stands in for the
-- /singletons/ 'SSymbol' and 'Data.Singleton.Sing' types.
data SSymbol n = KnownSymbol n => SSymbol

deriving instance Eq (SSymbol n)
deriving instance Ord (SSymbol n)

instance Show (SSymbol n) where
    showsPrec d x@SSymbol = showParen (d > 10) $
      showString "SSymbol @" . showsPrec 11 (fromSSymbol x)

instance GShow SSymbol where
    gshowsPrec = showsPrec

instance TestEquality SSymbol where
    testEquality (SSymbol :: SSymbol n) (SSymbol :: SSymbol m) =
      flip fmap (sameSymbol (Proxy :: Proxy n) (Proxy :: Proxy m)) $ \case
        Refl -> Refl

instance GEq SSymbol where
    geq = testEquality

instance GCompare SSymbol where
    gcompare x y = case compare (fromSSymbol x) (fromSSymbol y) of
      LT -> GLT
      EQ -> unsafeCoerce GEQ
      GT -> GGT

data SomeSymbol__ = forall n. SomeSymbol__ (SSymbol n)

-- | A useful pattern synonym for matching on a 'SomeSymbol' as if it
-- contained an @'SSymbol' n@, and not a @'Proxy' n@ as it exists in
-- "GHC.TypeLits".
--
-- A layer of compatibility letting us use the original 'SomeSymbol' type in
-- a way that works well with 'SSymbol'.
--
-- This stands in for the /singletons/ 'Data.Singleton.SomeSing' constructor.
pattern SomeSymbol_ :: SSymbol n -> SomeSymbol
pattern SomeSymbol_ x <- ((\case SomeSymbol (Proxy :: Proxy n) -> SomeSymbol__ (SSymbol :: SSymbol n)) -> SomeSymbol__ x)
  where
    SomeSymbol_ (SSymbol :: SSymbol n) = SomeSymbol (Proxy :: Proxy n)
{-# COMPLETE SomeSymbol_ #-}

-- | A useful pattern synonym for matching on a 'String' as if it "were"
-- a 'SSymbol':
--
-- @
-- myFunc :: String -> Bool
-- myFunc (FromSSymbol x) = ...  -- x is `SSymbol n`, with `n` coming from the input
-- @
--
-- It can be used as a function, as well, to convert an @'SSymbol' n@ back
-- into the 'String' that it represents.
--
-- This stands in for the /singletons/ 'Data.Singleton.FromSing' pattern synonym, except
-- it matches on a 'String' instead of a 'Data.Text.Text'.
pattern FromSSymbol :: SSymbol n -> String
pattern FromSSymbol x <- ((\i -> withSomeSymbol i SomeSymbol_) -> SomeSymbol_ x)
  where
    FromSSymbol = fromSSymbol
{-# COMPLETE FromSSymbol #-}

-- | Given an @'SSymbol' n@ and a value that would require a @'KnownSymbol' n@
-- instance, create that value.
--
-- This stands in for the function of the same name from
-- "Data.Singletons.TypeLits".
withKnownSymbol :: SSymbol n -> (KnownSymbol n => r) -> r
withKnownSymbol SSymbol x = x

-- | Promote ("reify") a 'String' to an @'SSymbol' n@, by providing
-- a continuation that would handle it in a way that is polymorphic over
-- all possible @n@.
--
-- This stands in the /singletons/ 'Data.Singleton.withSomeSing' function, except it takes
-- a 'String' instead of 'Data.Text.Text'.
withSomeSymbol :: String -> (forall n. SSymbol n -> r) -> r
withSomeSymbol (someSymbolVal->SomeSymbol (Proxy :: Proxy n)) x = x (SSymbol :: SSymbol n)

-- | Promote ("reify") a 'String' to an @'SSymbol' n@ existentially hidden
-- inside a 'SomeNat'.  To use it, pattern match using 'SomeSymbol_'.
--
-- This stands in the /singletons/ 'Data.Singleton.toSomeSing' function, except it takes
-- a 'String' instead of 'Data.Text.Text'.
toSomeSymbol :: String -> SomeSymbol
toSomeSymbol = someSymbolVal

-- | Convert ("reflect") an 'SSymbol' back into the 'String' it represents.
--
-- This stands in the /singletons/ 'Data.Singleton.fromSing' function, except it returns
-- a 'String' instead of 'Data.Text.Text'.
fromSSymbol :: SSymbol n -> String
fromSSymbol x@SSymbol = symbolVal x
