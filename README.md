typelits-witnesses
==================

[![typelits-witnesses on Hackage](https://img.shields.io/hackage/v/lens.svg?maxAge=2592000)](https://hackage.haskell.org/package/typelits-witnesses)
[![typelits-witnesses on Stackage LTS](http://stackage.org/package/typelits-witnesses/badge/lts)](http://stackage.org/lts/package/typelits-witnesses)
[![typelits-witnesses on Stackage Nightly](http://stackage.org/package/typelits-witnesses/badge/nightly)](http://stackage.org/nightly/package/typelits-witnesses)
[![Build
Status](https://travis-ci.org/mstksg/typelits-witnesses.svg?branch=master)](https://travis-ci.org/mstksg/typelits-witnesses)


Provides witnesses for `KnownNat` and `KnownSymbol` instances for various
operations on GHC TypeLits --- in particular, the arithmetic operations
defined in `GHC.TypeLits`, and also for type-level lists of `KnownNat` and
`KnownSymbol` instances.

This is useful for situations where you have `KnownNat n`, and you want to
prove to GHC `KnownNat (n + 3)`, or `KnownNat (2*n + 4)`.

It's also useful for when you want to work with type level lists of
`KnownNat`/`KnownSymbol` instances and singletons for traversing them, and be
able to apply analogies of `natVal`/`symbolVal` to lists with analogies for
`SomeNat` and `SomeSymbol`.

Note that most of the functionality in this library can be reproduced in a more
generic way using the great [singletons][] library.  The versions here are
provided as a "plumbing included" alternative that makes some commonly found
design patterns involving GHC's TypeLits functionality a little smoother ---
especially working with its `Nat` comparison API.

[singletons]: https://hackage.haskell.org/package/singletons


`GHC.TypeLits.Witnesses`
------------------------

Provides witnesses for instances arising from the arithmetic operations
defined in `GHC.TypeLits`.

In general, if you have `KnownNat n`, GHC can't infer `KnownNat (n + 1)`;
and if you have `KnownNat m`, as well, GHC can't infer `KnownNat (n + m)`.

This can be extremely annoying when dealing with libraries and applications
where one regularly adds and subtracts type-level nats and expects `KnownNat`
instances to follow.  For example, vector concatenation of length-encoded
vector types can be:

~~~haskell
concat :: (KnownNat n, KnownNat m)
       => Vector n       a
       -> Vector m       a
       -> Vector (n + m) a
~~~

But, `n + m` now does not have a `KnownNat` instance, which severely hinders
what you can do with this!

Consider this concrete (but silly) example:

~~~haskell
getDoubled :: KnownNat n => Proxy n -> Integer
getDoubled p = natVal (Proxy :: Proxy (n * 2))
~~~

Which is supposed to call `natVal` with `n * 2`.  However, this fails, because
while `n` is a `KnownNat`, `n * 2` is not necessarily so.  This module lets
you re-assure GHC that this is okay.

The most straightforward/high-level usage is with `withNatOp`:

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
in the module.  See module documentation for more detail.

`GHC.TypeLits.Compare`
----------------------

Provides tools for refining upper and lower bounds on `KnownNat`s and proving
inequalities involving *GHC.TypeLits*'s comparison API. (Both with `<=?` and
`CmpNat`).

If a library function requires `1 <= n` constraint, but only `KnownNat n` is
available:

~~~haskell
foo :: (KnownNat n, 1 <= n) => Proxy n -> Int

bar :: KnownNat n => Proxy n -> Int
bar n = case (Proxy :: Proxy 1) %<=? n of
          LE  Refl -> foo n
          NLE _    -> 0
~~~

`foo` requires that `1 <= n`, but `bar` has to handle all cases of `n`.  `%<=?`
lets you compare the `KnownNat`s in two `Proxy`s and returns a `:<=?`, which
has two constructors, `LE` and `NLE`.

If you pattern match on the result, in the `LE` branch, the constraint
`1 <= n` will be satisfied according to GHC, so `bar` can safely call
`foo`, and GHC will recognize that `1 <= n`.

In the `NLE` branch, the constraint that `1 > n` is satisfied, so any
functions that require that constraint would be callable.

For convenience, `isLE` and `isNLE` are also offered:

~~~haskell
bar :: KnownNat n => Proxy n -> Int
bar n = case isLE (Proxy :: Proxy 1) n of
          Just Refl -> foo n
          Nothing   -> 0
~~~

Similarly, if a library function requires something involving `CmpNat`,
you can use `cmpNat` and the `SCmpNat` type:

~~~haskell
foo1 :: (KnownNat n, CmpNat 5 n ~ LT) => Proxy n -> Int
foo2 :: (KnownNat n, CmpNat 5 n ~ GT) => Proxy n -> Int

bar :: KnownNat n => Proxy n -> Int
bar n = case cmpNat (Proxy :: Proxy 5) n of
          CLT Refl -> foo1 n
          CEQ Refl -> 0
          CGT Refl -> foo2 n
~~~

You can use the `Refl` that `cmpNat` gives you with `flipCmpNat` and
`cmpNatLE` to "flip" the inequality or turn it into something compatible
with `<=?` (useful for when you have to work with libraries that mix the
two methods) or `cmpNatEq` and `eqCmpNat` to get to/from witnesses for
equality of the two `Nat`s.

`GHC.TypeLits.List`
-------------------

Provides analogies of `KnownNat`, `SomeNat`, `natVal`, etc., to type-level
lists of `KnownNat` instances, and also singletons for iterating over
type-level lists of `Nat`s and `Symbol`s.

If you had `KnownNats ns`, then you have two things you can do with it; first,
`natsVal`, which is like `natVal` but for type-level lists of `KnownNats`:

~~~haskell
> natsVal (Proxy :: Proxy [1,2,3])
[1,2,3]
~~~

And more importantly, `natsList`, which provides singletons that you can
pattern match on to "reify" the structure of the list, getting a `Proxy n` for
every item in the list with a `KnownNat`/`KnownSymbol` instance in scope for
you to use:

~~~haskell
printNats :: NatList ns -> IO ()
printNats nl = case nl of
                 ØNL       ->
                   return ()
                 p :># nl' -> do
                   print $ natVal p
                   printNats nl'
~~~

~~~haskell
> printNats (natsList :: NatList [1,2,3])
1
2
3
~~~

Without this, there is no way to "iterate over" and "access" every `Nat` in a
list of `KnownNat`s.  You can't "iterate" over `[1,2,3]` in `Proxy [1,2,3]`,
but you can iterate over them in `NatList [1,2,3]`.

This module also lets you "reify" lists of `Integer`s or `String`s into
`NatList`s and `SymbolList`s, so you can access them at the type level for
some dependent types fun.

~~~haskell
> reifyNats [1,2,3] $ \nl -> do
    print nl
    printNats nl
Proxy :<# Proxy :<# Proxy :<# ØNL
1
2
3
~~~

Another thing you can do is provide witneses that two `[Nat]`s or `[Symbol]`s
are the same/were instantiated with the same numbers/symbols.

~~~haskell
> reifyNats [1,2,3] $ \ns -> do
  reifyNats [1,2,3] $ \ms -> do
    case sameNats ns ms of
      Just Refl -> -- in this branch, ns and ms are the same.
      Nothing   -> -- in this branch, they aren't
~~~

The above would match on the `Just Refl` branch.

See module documentation for more details and variations.

