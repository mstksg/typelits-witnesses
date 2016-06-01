{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module GHC.TypeLits.Compare
  ( (%<=?)
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
             Right Refl -> Just Refl
             Left _     -> Nothing

isNLE
    :: (KnownNat m, KnownNat n)
    => Proxy m
    -> Proxy n
    -> Maybe ((m <=? n) :~: 'False)
isNLE m n = case m %<=? n of
              Left Refl -> Just Refl
              Right _   -> Nothing

(%<=?)
     :: (KnownNat m, KnownNat n)
     => Proxy m
     -> Proxy n
     -> Either ((m <=? n) :~: 'False) ((m <=? n) :~: 'True)
m %<=? n | natVal m <= natVal n = Right (unsafeCoerce Refl)
         | otherwise            = Left (unsafeCoerce Refl)

