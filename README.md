typelits-witnesses
==================

[![typelits-witnesses on Hackage](https://img.shields.io/hackage/v/typelits-witnesses.svg?maxAge=2592000)](https://hackage.haskell.org/package/typelits-witnesses)
[![typelits-witnesses on Stackage LTS](http://stackage.org/package/typelits-witnesses/badge/lts)](http://stackage.org/lts/package/typelits-witnesses)
[![typelits-witnesses on Stackage Nightly](http://stackage.org/package/typelits-witnesses/badge/nightly)](http://stackage.org/nightly/package/typelits-witnesses)
[![Build Status](https://travis-ci.org/mstksg/typelits-witnesses.svg?branch=master)](https://travis-ci.org/mstksg/typelits-witnesses)

This library contains:

*   A small specialized subset of the *[singletons][]* library as it pertains to
    `Nat` and `Symbol`, for when you need some simple functionality without
    wanting to invoke the entire *singletons* library.
*   Operations for manipulating these singletons and `KnownNat` and
    `KnownSymbol` instances, such as addition and multiplication of
    singletons/`KnownNat` instances.
*   Operations for the comparison of `Nat`s in a way that works well with
    *GHC.TypeLits*'s different comparison systems.  This is helpful for
    bridging together libraries that use different systems; this functionality
    is not yet provided by *singletons*.

[singletons]: https://hackage.haskell.org/package/singletons
