| title: Announcing: Snap Framework v0.5.0
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-06-18T17:18:00-0400
| updated: 2011-06-18T17:18:00-0400
| summary: Release notes for Snap 0.5.0

The Snap team is happy to announce the release of Snap 0.5.0, a new version of
the Snap Framework. The 0.5 series does not contain many new features, but
there are some backwards-incompatible changes which necessitated an increase in
the minor version number.

## New features

  - We now ship a
    [Readable](https://github.com/snapframework/snap-core/blob/0.5.0/src/Snap/Util/Readable.hs)
    typeclass to make converting path segments and cookie values to ints or
    bytestrings more convenient.


## Incompatible changes

  - The implementation of
    [Snap.Http.Server.Config](https://github.com/snapframework/snap-server/blob/0.5.0/src/Snap/Http/Server/Config.hs)
    has been rewritten to be more sane, fixing
    [issue 73](https://github.com/snapframework/snap-core/issues/73).


## Improvements

  - The `snap` development loader now disables warnings and optimizations.
  
  - The Heist extension in the `snap` project (soon to be removed, anyways) got
    some new functions.


## Dependency changes

  - Snap now uses the
    [case-insensitive](http://hackage.haskell.org/package/case-insensitive)
    library for case-insensitive bytestrings instead of our own
    `Data.CIByteString` (which, in part, inspired the creation of that library)
    -- `case-insensitive` was created to unify the type of case-insensitive
    strings across Haskell projects.
