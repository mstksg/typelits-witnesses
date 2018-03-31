Version 0.3.0.2
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.3.0.2>

*March 30, 2018*

*   Functions taking multiple `p m` in *GHC.TypeLits.Compare* now are able to
    take different "proxy" types (`p` and `q`).

Version 0.3.0.1
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.3.0.1>

*   Documentation fixes because deprecation warnings were not showing up on
    haddocks for *GHC.TypeLits.List*.

Version 0.3.0.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.3.0.0>

*   Added extra witnesses inside the constructors of `(:<=?)`.
*   Allowed functions to polymorphically expect `p n` instead of `Proxy n`
    whenever possible.
*   Soft deprecation of *GHC.TypeLits.Witnesses*, in case people want to use
    the functionality of *singletons* without the full library.
*   Formal deprecation of *GHC.TypeLits.List*, with migration information,
    because it's just so much more unweidly than using *singletons*.

Version 0.2.3.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.2.3.0>

*   Added the `GHC.TypeLits.Compare` module for refining bounds and proving
    inequalities on `KnownNat`s and associated utility functions.

Version 0.2.2.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.2.2.0>

*   Removed redundant `KnownNats` and `KnownSymbols` constraints for `sameNats`
    and `sameSymbols`.

Version 0.2.1.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.2.1.0>

*   Added "eliminators", a staple of dependently typed programming, for
    `NatList` and `SymbolList`.

Version 0.2.0.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.2.0.0>

*   **Breaking**: Changed the name of `someNatsVal'` to `someNatsValPos`, to
    break away from the "just add `'`" anti-pattern and to make the function
    name a bit more meaningful.

*   Added `reifyNats'`, a "safe" version of `reifyNats`.  Ideally,
    `reifyNats` should be the safe one, but its connection to `reifyNat` from
    the *reflection* package is very strong and worth preserving, I think.

Version 0.1.2.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.2.0>

*   Added `mapNatList'` and `mapSymbolList'` companions to `mapNatList` and
    `mapSymbolList`; they use `NatList` and `SymbolList` instead of Rank-2
    types, so they can work better with function composition with `(.)` and
    other things that Rank-2 types would have trouble with.

*   Added `sameNats` and `sameSymbols`, modeled after `sameNat` and
    `sameSymbol`.  They provide witnesses to GHC that `KnownNat`s passed in
    are both the same.

Version 0.1.1.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.1.0>

*   Added strict fields to `NatList`, `SomeNats`, `SymbolList`, and
    `SomeSymbols`.  It really doesn't make any sense for them to be lazy.

Version 0.1.0.1
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.0.1>

*   Added README to the cabal package as an extra source file, for viewing on
    Hackage.

Version 0.1.0.0
===============

<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.0.0>

*   Initial version.

