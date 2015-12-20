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
  -- * @KnownNats@
    KnownNats(..)
  , SomeNats(..)
  , NatList(..)
  , someNatsVal
  , someNatsVal'
  , reifyNats
  , traverseNatList
  , traverseNatList_
  , mapNatList
  -- * @KnownSymbols@
  , KnownSymbols(..)
  , SomeSymbols(..)
  , someSymbolsVal
  , reifySymbols
  , traverseSymbolList
  , traverseSymbolList_
  , mapSymbolList
  ) where

import Data.Proxy
import Data.Reflection
import GHC.Exts        (Constraint)
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
    SomeNats :: KnownNats ns => NatList ns -> SomeNats

-- | Singleton-esque type for "traversing" over type-level lists of 'Nat's.
-- Essentially contains a (value-level) list of @'Proxy' n@s, but each 'n'
-- has a 'KnownNat' instance for you to use.  At runtime (after type
-- erasure), is more or less equivalent to a @['Integer']@.
--
-- Typically generated using 'natsList'.
data NatList :: [Nat] -> * where
    ØNL   :: NatList '[]
    (:<#) :: (KnownNat n, KnownNats ns) => Proxy n -> NatList ns -> NatList (n ': ns)

infixr 5 :<#
deriving instance Show (NatList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'NatList', each with the corresponding 'KnownNat' instance available.
-- Gives the the ability to "change" the represented natural number to
-- a new one, in a 'SomeNat'.
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
    merge :: forall ms. SomeNat -> SomeNats -> SomeNats
    merge sn sns = case sn of
                     SomeNat i ->
                       case sns of
                         SomeNats is ->
                           SomeNats (i :<# is)

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

-- | List equivalent of 'someNatVal'.  Convert a list of integers into an
-- unknown type-level list of naturals.  Will return 'Nothing' if any of
-- the given 'Integer's is negative.
someNatsVal :: [Integer] -> Maybe SomeNats
someNatsVal []     = Just (SomeNats ØNL)
someNatsVal (n:ns) = do
    SomeNat  m  <- someNatVal n
    SomeNats ms <- someNatsVal ns
    return $ SomeNats (m :<# ms)

-- | List equivalent of 'reifyNat'.  Give a list of integers and provide
-- a continuation from the generated 'NatList' from those integers, and
-- returns the result.
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
    SomeSymbols :: KnownSymbols ns => SymbolList ns -> SomeSymbols

-- | Singleton-esque type for "traversing" over type-level lists of
-- 'Symbol's. Essentially contains a (value-level) list of @'Proxy' n@s,
-- but each 'n' has a 'KnownSymbol' instance for you to use.  At runtime
-- (after type erasure), is more or less equivalent to a @['String']@.
--
-- Typically generated using 'symbolsList'.
data SymbolList :: [Symbol] -> * where
    ØSL   :: SymbolList '[]
    (:<$) :: (KnownSymbol n, KnownSymbols ns) => Proxy n -> SymbolList ns -> SymbolList (n ': ns)

infixr 5 :<$
deriving instance Show (SymbolList ns)

-- | Utility function for traversing over all of the @'Proxy' n@s in
-- a 'SymbolList', each with the corresponding 'KnownSymbol' instance
-- available.  Gives the the ability to "change" the represented natural
-- number to a new one, in a 'SomeSymbol'.
traverseSymbolList :: forall f ns. Applicative f
                   => (forall n. KnownSymbol n => Proxy n -> f String)
                   -> SymbolList ns
                   -> f SomeSymbols
traverseSymbolList f = go
  where
    go :: forall ms. SymbolList ms -> f SomeSymbols
    go nl = case nl of
              ØSL       -> pure $ SomeSymbols ØSL
              p :<$ nl' -> merge <$> f p <*> go nl'
    merge :: forall ms. String -> SomeSymbols -> SomeSymbols
    merge s sn = reifySymbol s $ \p ->
                   case sn of
                     SomeSymbols sn' -> SomeSymbols (p :<$ sn')

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
mapSymbolList :: forall ns.
                 (forall n. KnownSymbol n => Proxy n -> String)
              -> SymbolList ns
              -> SomeSymbols
mapSymbolList f = runIdentity . traverseSymbolList (Identity . f)

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

-- | List equivalent of 'reifySymbol'.  Give a list of strings and provide
-- a continuation from the generated 'SymbolList' from those integers, and
-- returns the result.
reifySymbols :: [String] -> (forall ns. KnownSymbols ns => SymbolList ns -> r) -> r
reifySymbols []     f = f ØSL
reifySymbols (n:ns) f = reifySymbol n $ \m ->
                          reifySymbols ns $ \ms ->
                            f (m :<$ ms)
