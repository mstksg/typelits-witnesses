typelits-witnesses
==================

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

The example seems a bit silly, but it's useful for libraries with things like
"statically sized vectors"; if you want to concatenate a `Vec n` and a `Vec m`
and get a `Vec (n + m)` polymorphically, you're going to need something like
this to be able to use your `Vec (n + m)` as if `n + m` was an instance of
`KnownNat`.

There are four "nat operations" defined here, corresponding to the four
type-level operations on `Nat` provided in `GHC.TypeLits`: `(%+)`, `(%-)`,
`(%*)`, and `(%^)`, corresponding to addition, subtraction, multiplication,
and exponentiation, respectively.

Note that `(%-)` is implemented in a way that allows for the result to be a
*negative* `Nat`.

There are more advanced operations dealing with low-level machinery, as well,
in the module.  See module documentation for more detail.

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
pattern match on to "reify" the structure of the list:

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
some dependent types fun.  When traversing over a `NatList` or `SymbolList`,
you get a `Proxy n` for each item in the list, with a `KnownNat`/`KnownSymbol`
instance available to use.

~~~haskell
> reifyNats [1,2,3] $ \nl -> do
    print nl
    printNats nl
Proxy :<# Proxy :<# Proxy :<# ØNL
1
2
3
~~~

See module documentation for more details and variations.

