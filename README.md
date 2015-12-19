typelits-witnesses
==================

Provides witnesses for `KnownNat` and `KnownSymbol` instances for various
operations on GHC TypeLits --- in particular, the arithmetic operations
defined in `GHC.TypeLits`, and also for type-level lists of `KnownNat` and
`KnownSymbol` instances.

This is useful for situations where you have `KnownNat n`, and you want to
prove to GHC `KnownNat (n + 3)`, or `KnownNat (2*n + 4)`.

It's also useful for when you want to work with type level lists of
`KnownNat`/`KnownSymbol` instances, and be able to apply analogies of
`natVal`/`symbolVal` to lists with analogies for `SomeNat` and `SomeSymbol`.

`GHC.TypeLits.Witnesses`
------------------------

Provides witnesses for instances arising from the arithmetic operations
defined in `GHC.TypeLits`.  Consider the common (simplified) problem:

~~~haskell
getDoubled :: KnownNat n => Proxy n -> Integer
getDoubled p = natVal (Proxy :: Proxy (n * 2))
~~~

Which is supposed to call `natVal` with `n * 2`.  However, this fails, because
while `n` is a `KnownNat`, `n * 2` is not necessarily so.  This module lets
you re-assure GHC that this is okay.

The most straightforward usage is with `withNatOp`:

~~~haskell
getDoubled :: forall n. KnownNat n => Proxy n -> Integer
getDoubled p = withNatOp (%*) p (Proxy :: Proxy 2) $
    natVal (Proxy :: Proxy (n * 2))
~~~

Within the scope of the argument of
`withNatOp (%*) (Proxy :: Proxy n) (Proxy :: Proxy m)`, `n * m` is an instance
of `KnownNat`, so you can use `natVal` on it, and get the expected result:

~~~haskell
> getDoubled (Proxy :: Proxy 12)
24
~~~

There are four "nat operations" defined here, corresponding to the four
type-level operations on `Nat` provided in `GHC.TypeLits`: `(%+)`, `(%-)`,
`(%*)`, and `(%^)`, corresponding to addition, subtraction, multiplication,
and exponentiation, respectively.

Note that `(%-)` is implemented in a way that allows for the result to be a
*negative* `Nat`.

There are more advanced operations dealing with low-level machinery, as well,
in the module.

`GHC.TypeLits.List`
-------------------

Provides analogies of `KnownNat`, `SomeNat`, `natVal`, etc., to type-level
lists of `KnownNat` instances.


