{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : GHC.TypeLits.List
-- Description : Typeclasses, singletons, and reifiers for type-level lists
--               of 'Nat's and 'Symbol's.
-- Copyright   : (c) Justin Le 2016
-- License     : MIT
-- Maintainer  : justin@jle.im
-- Stability   : unstable
-- Portability : non-portable
--
--
-- Provides the 'KnownNats' and 'KnownSymbols' typeclasses in analogy to
-- 'KnownNat' and 'KnownSymbol' from "GHC.TypeLits".  Also provides
-- singleton-esque structures for traversing over type-level lists of
-- 'Nat's and 'Symbol's.  Comes with continuation-style reifiers and
-- existential types for dependent typing usage, and as an analogy with
-- 'SomeNat' and 'SomeSymbol'.
--
-- See typeclass documentations and README for more information.

module GHC.TypeLits.List
  {-# DEPRECATED "Use singletons package instead" #-} (
  -- * 'KnownNats'
    KnownNats(..)
  , SomeNats(..)
  , NatList(..)
  , someNatsVal
  , someNatsValPos
  , reifyNats
  , reifyNats'
  , sameNats
  , elimNatList
  -- ** Traversals
  , traverseNatList
  , traverseNatList'
  , traverseNatList_
  -- *** Maps
  , mapNatList
  , mapNatList'
  -- * 'KnownSymbols'
  , KnownSymbols(..)
  , SomeSymbols(..)
  , SymbolList(..)
  , someSymbolsVal
  , reifySymbols
  , sameSymbols
  , elimSymbolList
  -- ** Traversals
  , traverseSymbolList
  , traverseSymbolList'
  , traverseSymbolList_
  -- *** Maps
  , mapSymbolList
  , mapSymbolList'
  ) where

import Data.Functor.Identity
import Data.Proxy
import Prelude.Compat
import Data.Reflection
import Data.Type.Equality
import GHC.TypeLits


-- | @'KnownNats' ns@ is intended to represent that every 'Nat' in the
-- type-level list 'ns' is itself a 'KnownNat' (meaning, you can use
-- 'natVal' to get its corresponding 'Integer').
--
-- In practice, just knowing that every item has a 'KnownNat' instance is
-- not enough; it's nice, but unless you're able to "iterate" over every
-- 'Nat' in the list, it's of limited use.  That's why this class also
-- provides a constructor for @'NatList' ns@, so that you can produce
-- a 'NatList' for every @'KnownNat' ns@, which you can iterate over to get
-- @'Proxy' n@s for every 'n' in 'ns' along with the @'KnownNat' n@
-- instances.
--
-- It also has an analogy to 'natVal', 'natsVal', which lets you get a list
-- of the represented 'Integer's for, say, @'Proxy' [1,2,3]@.
class KnownNats (ns :: [Nat]) where
    natsVal  :: p ns -> [Integer]
    natsList :: NatList ns
{-# DEPRECATED KnownNats "Use SingI from the singletons package instead" #-}
{-# DEPRECATED natsVal "Use fromSing from the singletons package instead" #-}
{-# DEPRECATED natsList "Use sing from the singletons package instead" #-}

instance KnownNats '[] where
    natsVal  _ = []
    natsList   = ØNL

instance (KnownNat n, KnownNats ns) => KnownNats (n ': ns) where
    natsVal  _ = natVal (Proxy :: Proxy n) : natsVal (Proxy :: Proxy ns)
    natsList   = Proxy :<# natsList

-- | Represents unknown type-level lists of type-level natural numbers.
-- It's a 'NatList', but you don't know what the list contains at
-- compile-time.
data SomeNats :: * where
    SomeNats :: KnownNats ns => !(NatList ns) -> SomeNats
{-# DEPRECATED SomeNats "Use SomeSing from the singletons package instead" #-}

-- | Singleton-esque type for "traversing" over type-level lists of 'Nat's.
-- Essentially contains a (value-level) list of @'Proxy' n@s, but each 'n'
-- has a 'KnownNat' instance for you to use.  At runtime (after type
-- erasure), is more or less equivalent to a @['Integer']@.
--
-- Typically generated using 'natsList'.
data NatList :: [Nat] -> * where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns)
          => !(Proxy n) -> !(NatList ns) -> NatList (n ': ns)
{-# DEPRECATED NatList "Use Sing from the singletons package instead" #-}

infixr 5 :<#
deriving instance Show (NatList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeNat'.
--
-- Can be considered a form of a @Traversal' 'SomeNats' 'SomeNat'@.
traverseNatList
    :: forall f ns. Applicative f
    => (forall n. KnownNat n => Proxy n -> f SomeNat)
    -> NatList ns
    -> f SomeNats
traverseNatList f = go
  where
    go :: forall ms. NatList ms -> f SomeNats
    go = \case
      ØNL      -> pure $ SomeNats ØNL
      n :<# ns -> merge <$> f n <*> go ns
    merge :: SomeNat -> SomeNats -> SomeNats
    merge = \case
      SomeNat n -> \case
        SomeNats ns ->
          SomeNats (n :<# ns)

-- | Like 'traverseNatList', but literally actually a @Traversal'
-- 'SomeNats' 'SomeNat'@, avoiding the Rank-2 types, so is usable with
-- lens-library machinery.
traverseNatList'
    :: forall f. Applicative f
    => (SomeNat -> f SomeNat)
    -> SomeNats
    -> f SomeNats
traverseNatList' f = \case
    SomeNats ns -> traverseNatList (f . SomeNat) ns

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Results are ignored.
traverseNatList_
    :: forall f a ns. Applicative f
    => (forall n. KnownNat n => Proxy n -> f a)
    -> NatList ns
    -> f ()
traverseNatList_ f = go
  where
    go :: forall ms. NatList ms -> f ()
    go = \case
      ØNL       -> pure ()
      n :<# ns -> f n *> go ns

-- | The "eliminator" for 'NatList'.  You can think of this as
-- a dependently typed analogy for a fold.
--
-- /Since 0.2.1.0/
elimNatList
    :: forall p ns. ()
    => p '[]
    -> (forall m ms. (KnownNat m, KnownNats ms) => Proxy m -> p ms -> p (m ': ms))
    -> NatList ns
    -> p ns
elimNatList z s = \case
    ØNL      -> z
    n :<# ns -> s n (elimNatList z s ns)


-- | Utility function for \"mapping\" over each of the 'Nat's in the
-- 'NatList'.
mapNatList
    :: (forall n. KnownNat n => Proxy n -> SomeNat)
    -> NatList ns
    -> SomeNats
mapNatList f = runIdentity . traverseNatList (Identity . f)

-- | Like 'mapNatList', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapNatList'
-- would cause problems.
mapNatList'
    :: (SomeNat -> SomeNat)
    -> SomeNats
    -> SomeNats
mapNatList' f = runIdentity . traverseNatList' (Identity . f)

-- | List equivalent of 'someNatVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Integer's is negative.
someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal []     = Just (SomeNats ØNL)
someNatsVal (n:ns) = do
    SomeNat  m  <- someNatVal n
    SomeNats ms <- someNatsVal ns
    return $ SomeNats (m :<# ms)
{-# DEPRECATED someNatsVal "Use toSing from the singletons package instead" #-}

-- | List equivalent of 'reifyNat'.  Given a list of integers, takes
-- a function in an "environment" with a @'NatList' ns@ corresponding to
-- the given list, where every @n@ in @ns@ has a 'KnownNat' instance.
--
-- Essentially a continuation-style version of 'SomeNats'.
--
-- Be aware that this also produces @'KnownNat' n@s where @n@ is negative,
-- without complaining.  To be consistent, within the library, this
-- /should/ be called @reifyNatsPos@; however, the naming choice is for
-- consistency with 'reifyNat' from the /reflections/ package.  Use
-- 'reifyNats'' for a "safe" version.
reifyNats :: [Integer] -> (forall ns. KnownNats ns => NatList ns -> r) -> r
reifyNats []     f = f ØNL
reifyNats (n:ns) f = reifyNat n $ \m ->
                       reifyNats ns $ \ms ->
                         f (m :<# ms)
{-# DEPRECATED reifyNats "Use withSomeSing from the singletons package instead" #-}

-- | "Safe" version of 'reifyNats', which will only run the continuation if
-- every 'Integer' in the list is non-negative.  If not, then returns
-- the given "default" value instead.
reifyNats'
    :: [Integer]
    -> r
    -> (forall ns. KnownNats ns => NatList ns -> r)
    -> r
reifyNats' ns d f =
    case someNatsVal ns of
      Just (SomeNats ms) -> f ms
      Nothing            -> d
{-# DEPRECATED reifyNats' "Use withSomeSing from the singletons package instead" #-}

-- | Like 'someNatsVal', but will also go ahead and produce 'KnownNat's
-- whose integer values are negative.  It won't ever error on producing
-- them, but extra care must be taken when using the produced 'SomeNat's.
someNatsValPos :: [Integer] -> SomeNats
someNatsValPos ns = reifyNats ns SomeNats
{-# DEPRECATED someNatsValPos "Use toSing from the singletons package instead" #-}

-- | Get evidence that the two 'KnownNats' lists are actually the "same"
-- list of 'Nat's (that they were instantiated with the same numbers).
--
-- Essentialy runs 'sameNat' over the lists:
--
-- @
-- case 'sameNats' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the two ['Nat']s
--                  -- are the same.
--   Nothing   -> -- in this branch, they aren't
-- @
sameNats
    :: NatList ns
    -> NatList ms
    -> Maybe (ns :~: ms)
sameNats = \case
    ØNL      -> \case
      ØNL      -> Just Refl
      _ :<# _  -> Nothing
    n :<# ns -> \case
      ØNL      -> Nothing
      m :<# ms -> do
        Refl <- sameNat n m
        Refl <- sameNats ns ms
        return Refl
{-# DEPRECATED sameNats "Use (%~) from the singletons package instead" #-}


-- | @'KnownSymbols' ss@ is intended to represent that every 'Symbol' in the
-- type-level list 'ss' is itself a 'KnownSymbol' (meaning, you can use
-- 'symbolVal' to get its corresponding 'String').
--
-- You can use 'symbolsVal' to get the corresponding @['String']@ from
-- @'KnownSymbols' ss@.
--
-- For reasons discussed further in the documentation for 'KnownNats', this
-- also lets you generate a @'SymbolList' ss@, in order to iterate over the
-- type-level list of 'Symbol's and take advantage of their 'KnownSymbol'
-- instances.
class KnownSymbols (ss :: [Symbol]) where
    symbolsVal  :: p ss -> [String]
    symbolsList :: SymbolList ss
{-# DEPRECATED KnownSymbols "Use SingI from the singletons package instead" #-}
{-# DEPRECATED symbolsVal "Use fromSing from the singletons package instead" #-}
{-# DEPRECATED symbolsList "Use sing from the singletons package instead" #-}

instance KnownSymbols '[] where
    symbolsVal  _ = []
    symbolsList    = ØSL

instance (KnownSymbol s, KnownSymbols ss) => KnownSymbols (s ': ss) where
    symbolsVal  _ = symbolVal (Proxy :: Proxy s) : symbolsVal (Proxy :: Proxy ss)
    symbolsList   = Proxy :<$ symbolsList

-- | Represents unknown type-level lists of 'Symbol's. It's a 'SymbolList',
-- but you don't know what the list contains at compile-time.
data SomeSymbols :: * where
    SomeSymbols :: KnownSymbols ss => !(SymbolList ss) -> SomeSymbols
{-# DEPRECATED SomeSymbols "Use SomeSing from the singletons package instead" #-}

-- | Singleton-esque type for "traversing" over type-level lists of
-- 'Symbol's. Essentially contains a (value-level) list of @'Proxy' n@s,
-- but each 'n' has a 'KnownSymbol' instance for you to use.  At runtime
-- (after type erasure), is more or less equivalent to a @['String']@.
--
-- Typically generated using 'symbolsList'.
data SymbolList :: [Symbol] -> * where
    ØSL   :: SymbolList '[]
    (:<$) :: (KnownSymbol s, KnownSymbols ss)
          => !(Proxy s) -> !(SymbolList ss) -> SymbolList (s ': ss)
{-# DEPRECATED SymbolList "Use Sing from the singletons package instead" #-}

infixr 5 :<$
deriving instance Show (SymbolList ns)

-- | Utility function for traversing over all of the @'Proxy' s@s in
-- a 'SymbolList', each with the corresponding 'KnownSymbol' instance
-- available.  Gives the the ability to "change" the represented natural
-- number to a new one, in a 'SomeSymbol'.
--
-- Can be considered a form of a @Traversal' 'SomeSymbols' 'SomeSymbol'@.
traverseSymbolList
    :: forall f ss. Applicative f
    => (forall s. KnownSymbol s => Proxy s -> f SomeSymbol)
    -> SymbolList ss
    -> f SomeSymbols
traverseSymbolList f = go
  where
    go :: forall ms. SymbolList ms -> f SomeSymbols
    go = \case
      ØSL      -> pure $ SomeSymbols ØSL
      s :<$ ss -> merge <$> f s <*> go ss
    merge :: SomeSymbol -> SomeSymbols -> SomeSymbols
    merge = \case
      SomeSymbol s -> \case
        SomeSymbols ss ->
          SomeSymbols (s :<$ ss)

-- | Like 'traverseSymbolList', but literally actually a
-- @Traversal' 'SomeSymbols' 'SomeSymbol'@, avoiding the Rank-2 types, so
-- is usable with lens-library machinery.
traverseSymbolList'
    :: forall f. Applicative f
    => (SomeSymbol -> f SomeSymbol)
    -> SomeSymbols
    -> f SomeSymbols
traverseSymbolList' f = \case
    SomeSymbols ns' -> traverseSymbolList (f . SomeSymbol) ns'

-- | Utility function for traversing over all of the @'Proxy' s@s in
-- a 'SymbolList', each with the corresponding 'KnownSymbol' instance
-- available. Results are ignored.
traverseSymbolList_
    :: forall f ss. Applicative f
    => (forall s a. KnownSymbol s => Proxy s -> f a)
    -> SymbolList ss
    -> f ()
traverseSymbolList_ f = go
  where
    go :: forall ts. SymbolList ts -> f ()
    go = \case
      ØSL      -> pure ()
      s :<$ ss -> f s *> go ss

-- | Utility function for \"mapping\" over each of the 'Symbol's in the
-- 'SymbolList'.
mapSymbolList
    :: (forall s. KnownSymbol s => Proxy s -> SomeSymbol)
    -> SymbolList ss
    -> SomeSymbols
mapSymbolList f = runIdentity . traverseSymbolList (Identity . f)

-- | Like 'mapSymbolList', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapSymbolList'
-- would cause problems.
mapSymbolList'
    :: (SomeSymbol -> SomeSymbol)
    -> SomeSymbols
    -> SomeSymbols
mapSymbolList' f = runIdentity . traverseSymbolList' (Identity . f)

-- | The "eliminator" for 'SymbolList'.  You can think of this as
-- a dependently typed analogy for a fold.
--
-- /Since 0.2.1.0/
elimSymbolList
    :: forall p ss. ()
    => p '[]
    -> (forall t ts. (KnownSymbol t, KnownSymbols ts) => Proxy t -> p ts -> p (t ': ts))
    -> SymbolList ss
    -> p ss
elimSymbolList z s = \case
    ØSL      -> z
    n :<$ ns -> s n (elimSymbolList z s ns)


-- | List equivalent of 'someNatVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Integer's is negative.
someSymbolsVal :: [String] -> SomeSymbols
someSymbolsVal []     = SomeSymbols ØSL
someSymbolsVal (s:ss) =
    case someSymbolVal s of
      SomeSymbol t ->
        case someSymbolsVal ss of
          SomeSymbols ts ->
            SomeSymbols (t :<$ ts)
{-# DEPRECATED someSymbolsVal "Use toSing from the singletons package instead" #-}

-- | List equivalent of 'reifyNat'.  Given a list of integers, takes
-- a function in an "environment" with a @'SymbolList' ss@ corresponding to
-- the given list, where every @s@ in @ss@ has a 'KnownSymbol' instance.
--
-- Essentially a continuation-style version of 'SomeSymbols'.
reifySymbols :: [String]
             -> (forall ss. KnownSymbols ss => SymbolList ss -> r)
             -> r
reifySymbols []     f = f ØSL
reifySymbols (s:ss) f = reifySymbol s $ \t ->
                          reifySymbols ss $ \ts ->
                            f (t :<$ ts)
{-# DEPRECATED reifySymbols "Use withSomeSing from the singletons package instead" #-}


-- | Get evidence that the two 'KnownSymbols' lists are actually the "same"
-- list of 'Symboles's (that they were instantiated with the same strings).
--
-- Essentialy runs 'sameSymbol' over the lists:
--
-- @
-- case 'sameSymbols' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the
--                  -- two ['Symbol']s are the same
--   Nothing   -> -- in this branch, they aren't
-- @
sameSymbols
    :: SymbolList ns
    -> SymbolList ms
    -> Maybe (ns :~: ms)
sameSymbols = \case
    ØSL      -> \case
      ØSL      -> Just Refl
      _ :<$ _  -> Nothing
    s :<$ ss -> \case
      ØSL      -> Nothing
      t :<$ ts -> do
        Refl <- sameSymbol s t
        Refl <- sameSymbols ss ts
        return Refl
{-# DEPRECATED sameSymbols "Use (%~) from the singletons package instead" #-}

