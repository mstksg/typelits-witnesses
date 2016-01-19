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

-- |
-- Module      : GHC.TypeLits.List
-- Description : Typeclasses, singletons, and reifiers for type-level lists
--               of 'Nat's and 'Symbol's.
-- Copyright   : (c) Justin Le 2015
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
-- See typeclass documentations for more information.

module GHC.TypeLits.List (
  -- * 'KnownNats'
    KnownNats(..)
  , SomeNats(..)
  , NatList(..)
  , someNatsVal
  , someNatsVal'
  , reifyNats
  , sameNats
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
  -- ** Traversals
  , traverseSymbolList
  , traverseSymbolList'
  , traverseSymbolList_
  -- *** Maps
  , mapSymbolList
  , mapSymbolList'
  ) where

import Data.Proxy
import Data.Type.Equality
import Data.Reflection
import GHC.TypeLits
import Data.Functor.Identity

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

infixr 5 :<#
deriving instance Show (NatList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeNat'.
--
-- Can be considered a form of a @Traversal' 'SomeNat' 'SomeNats'@.
traverseNatList :: forall f ns. Applicative f
                => (forall n. KnownNat n => Proxy n -> f SomeNat)
                -> NatList ns
                -> f SomeNats
traverseNatList f = go
  where
    go :: forall ms. NatList ms -> f SomeNats
    go nl = case nl of
              ØNL       -> pure $ SomeNats ØNL
              p :<# nl' -> merge <$> f p <*> go nl'
    merge :: SomeNat -> SomeNats -> SomeNats
    merge sn sns = case sn of
                     SomeNat i ->
                       case sns of
                         SomeNats is ->
                           SomeNats (i :<# is)

-- | Like 'traverseNatList', but literally actually a @Traversal' 'SomeNat'
-- 'SomeNats'@, avoiding the Rank-2 types, so is usable with lens-library
-- machinery.
traverseNatList' :: forall f. Applicative f
                 => (SomeNat -> f SomeNat)
                 -> SomeNats
                 -> f SomeNats
traverseNatList' f ns =
    case ns of
      SomeNats ns' -> traverseNatList (f . SomeNat) ns'

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Results are ignored.
traverseNatList_ :: forall f a ns. Applicative f
                 => (forall n. KnownNat n => Proxy n -> f a)
                 -> NatList ns
                 -> f ()
traverseNatList_ f = go
  where
    go :: forall ms. NatList ms -> f ()
    go nl = case nl of
              ØNL       -> pure ()
              p :<# nl' -> f p *> go nl'

-- | Utility function for \"mapping\" over each of the 'Nat's in the
-- 'NatList'.
mapNatList :: (forall n. KnownNat n => Proxy n -> SomeNat)
           -> NatList ns
           -> SomeNats
mapNatList f = runIdentity . traverseNatList (Identity . f)

-- | Like 'mapNatList', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapNatList'
-- would cause problems.
mapNatList' :: (SomeNat -> SomeNat)
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

-- | List equivalent of 'reifyNat'.  Given a list of integers, takes
-- a function in an "environment" with a @'NatList' ns@ corresponding to
-- the given list, where every @n@ in @ns@ has a 'KnownNat' instance.
--
-- Essentially a continuation-style version of 'SomeNats'.
--
-- For compatability with 'reifyNat', be aware that this also produces
-- @'KnownNat' n@s where @n@ is negative, without complaining.
reifyNats :: [Integer] -> (forall ns. KnownNats ns => NatList ns -> r) -> r
reifyNats []     f = f ØNL
reifyNats (n:ns) f = reifyNat n $ \m ->
                       reifyNats ns $ \ms ->
                         f (m :<# ms)

-- | Like 'someNatsVal', but will also go ahead and produce 'KnownNat's
-- whose integer values are negative.  It won't ever error on producing
-- them, but extra care must be taken when using the produced 'SomeNat's.
someNatsVal' :: [Integer] -> SomeNats
someNatsVal' ns = reifyNats ns SomeNats

-- | Get evidence that the two 'KnownNats' lists are actually the "same"
-- list of 'Nat's (that they were instantiated with the same numbers).
--
-- Essentialy runs 'sameNat' over the lists:
--
-- @
-- case 'sameNats' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the two ['Nat']s
--                  -- are the same.
--   Nothing     -> -- in this branch, they aren't
-- @
sameNats :: (KnownNats ns, KnownNats ms)
         => NatList ns
         -> NatList ms
         -> Maybe (ns :~: ms)
sameNats ns ms =
    case ns of
      ØNL ->
        case ms of
          ØNL     -> Just Refl
          _ :<# _ -> Nothing
      n :<# ns' ->
        case ms of
          ØNL     -> Nothing
          m :<# ms' -> do
            Refl <- sameNat n m
            Refl <- sameNats ns' ms'
            return Refl



-- | @'KnownSymbols' ns@ is intended to represent that every 'Symbol' in the
-- type-level list 'ns' is itself a 'KnownSymbol' (meaning, you can use
-- 'symbolVal' to get its corresponding 'String').
--
-- You can use 'symbolsVal' to get the corresponding @['String']@ from
-- @'KnownSymbols' ns@.
--
-- For reasons discussed further in the documentation for 'KnownNats', this
-- also lets you generate a @'SymbolList' ns@, in order to iterate over the
-- type-level list of 'Symbol's and take advantage of their 'KnownSymbol'
-- instances.
class KnownSymbols (ns :: [Symbol]) where
    symbolsVal  :: p ns -> [String]
    symbolsList :: SymbolList ns

instance KnownSymbols '[] where
    symbolsVal  _ = []
    symbolsList    = ØSL

instance (KnownSymbol n, KnownSymbols ns) => KnownSymbols (n ': ns) where
    symbolsVal  _ = symbolVal (Proxy :: Proxy n) : symbolsVal (Proxy :: Proxy ns)
    symbolsList   = Proxy :<$ symbolsList

-- | Represents unknown type-level lists of 'Symbol's. It's a 'SymbolList',
-- but you don't know what the list contains at compile-time.
data SomeSymbols :: * where
    SomeSymbols :: KnownSymbols ns => !(SymbolList ns) -> SomeSymbols

-- | Singleton-esque type for "traversing" over type-level lists of
-- 'Symbol's. Essentially contains a (value-level) list of @'Proxy' n@s,
-- but each 'n' has a 'KnownSymbol' instance for you to use.  At runtime
-- (after type erasure), is more or less equivalent to a @['String']@.
--
-- Typically generated using 'symbolsList'.
data SymbolList :: [Symbol] -> * where
    ØSL   :: SymbolList '[]
    (:<$) :: (KnownSymbol n, KnownSymbols ns)
          => !(Proxy n) -> !(SymbolList ns) -> SymbolList (n ': ns)

infixr 5 :<$
deriving instance Show (SymbolList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'SymbolList', each with the corresponding 'KnownSymbol' instance
-- available.  Gives the the ability to "change" the represented natural
-- number to a new one, in a 'SomeSymbol'.
--
-- Can be considered a form of a @Traversal' 'SomeSymbol' 'SomeSymbols'@.
traverseSymbolList :: forall f ns. Applicative f
                   => (forall n. KnownSymbol n => Proxy n -> f SomeSymbol)
                   -> SymbolList ns
                   -> f SomeSymbols
traverseSymbolList f = go
  where
    go :: forall ms. SymbolList ms -> f SomeSymbols
    go nl = case nl of
              ØSL       -> pure $ SomeSymbols ØSL
              p :<$ nl' -> merge <$> f p <*> go nl'
    merge :: SomeSymbol -> SomeSymbols -> SomeSymbols
    merge s sl = case s of
                   SomeSymbol ps ->
                     case sl of
                       SomeSymbols sl' ->
                         SomeSymbols (ps :<$ sl')

-- | Like 'traverseSymbolList', but literally actually a
-- @Traversal' 'SomeSymbol' 'SomeSymbols'@, avoiding the Rank-2 types, so
-- is usable with lens-library machinery.
traverseSymbolList' :: forall f. Applicative f
                 => (SomeSymbol -> f SomeSymbol)
                 -> SomeSymbols
                 -> f SomeSymbols
traverseSymbolList' f ns =
    case ns of
      SomeSymbols ns' -> traverseSymbolList (f . SomeSymbol) ns'

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'SymbolList', each with the corresponding 'KnownSymbol' instance
-- available. Results are ignored.
traverseSymbolList_ :: forall f ns. Applicative f
                    => (forall n a. KnownSymbol n => Proxy n -> f a)
                    -> SymbolList ns
                    -> f ()
traverseSymbolList_ f = go
  where
    go :: forall ms. SymbolList ms -> f ()
    go nl = case nl of
              ØSL       -> pure ()
              p :<$ nl' -> f p *> go nl'

-- | Utility function for \"mapping\" over each of the 'Symbol's in the
-- 'SymbolList'.
mapSymbolList :: (forall n. KnownSymbol n => Proxy n -> SomeSymbol)
              -> SymbolList ns
              -> SomeSymbols
mapSymbolList f = runIdentity . traverseSymbolList (Identity . f)

-- | Like 'mapSymbolList', but avoids the Rank-2 types, so can be used with
-- '.' (function composition) and in other situations where 'mapSymbolList'
-- would cause problems.
mapSymbolList' :: (SomeSymbol -> SomeSymbol)
               -> SomeSymbols
               -> SomeSymbols
mapSymbolList' f = runIdentity . traverseSymbolList' (Identity . f)

-- | List equivalent of 'someNatVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Integer's is negative.
someSymbolsVal :: [String] -> SomeSymbols
someSymbolsVal []     = SomeSymbols ØSL
someSymbolsVal (n:ns) =
    case someSymbolVal n of
      SomeSymbol m ->
        case someSymbolsVal ns of
          SomeSymbols ms ->
            SomeSymbols (m :<$ ms)

-- | List equivalent of 'reifyNat'.  Given a list of integers, takes
-- a function in an "environment" with a @'NatList' ns@ corresponding to
-- the given list, where every @n@ in @ns@ has a 'KnownNat' instance.
--
-- Essentially a continuation-style version of 'SomeSymbols'.
reifySymbols :: [String]
             -> (forall ns. KnownSymbols ns => SymbolList ns -> r)
             -> r
reifySymbols []     f = f ØSL
reifySymbols (n:ns) f = reifySymbol n $ \m ->
                          reifySymbols ns $ \ms ->
                            f (m :<$ ms)

-- | Get evidence that the two 'KnownSymbols' lists are actually the "same"
-- list of 'Symboles's (that they were instantiated with the same strings).
--
-- Essentialy runs 'sameSymbol' over the lists:
--
-- @
-- case 'sameSymbols' ns ms of
--   Just 'Refl' -> -- in this branch, GHC recognizes that the
--                  -- two ['Symbol']s are the same
--   Nothing     -> -- in this branch, they aren't
-- @
sameSymbols :: (KnownSymbols ns, KnownSymbols ms)
            => SymbolList ns
            -> SymbolList ms
            -> Maybe (ns :~: ms)
sameSymbols ns ms =
    case ns of
      ØSL ->
        case ms of
          ØSL     -> Just Refl
          _ :<$ _ -> Nothing
      n :<$ ns' ->
        case ms of
          ØSL     -> Nothing
          m :<$ ms' -> do
            Refl <- sameSymbol n m
            Refl <- sameSymbols ns' ms'
            return Refl
