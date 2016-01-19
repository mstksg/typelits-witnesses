0.1.2.0
=======
<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.2.0>

*   Added `mapNatList'` and `mapSymbolList'` companions to `mapNatList` and
    `mapSymbolList`; they use `NatList` and `SymbolList` instead of Rank-2
    types, so they can work better with function composition with `(.)` and
    other things that Rank-2 types would have trouble with.

*   Added `sameNats` and `sameSymbols`, modeled after `sameNat` and
    `sameSymbol`.  They provide witnesses to GHC that `KnownNat`s passed in
    are both the same.

=======
<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.1.0>

*   Added strict fields to `NatList`, `SomeNats`, `SymbolList`, and
    `SomeSymbols`.  It really doesn't make any sense for them to be lazy.

0.1.0.1
=======
<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.0.1>

*   Added README to the cabal package as an extra source file, for viewing on
    Hackage.

0.1.0.0
=======
<https://github.com/mstksg/typelits-witnesses/releases/tag/v0.1.0.0>

*   Initial version.

