| title: Announcing: Snap Framework v0.5.3
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-08-13T10:01:00+0100
| updated: 2011-08-13T10:01:00+0100
| summary: Release notes for Snap 0.5.3

Hello from [CamHac](http://www.haskell.org/haskellwiki/CamHac)! The Snap team
is happy to announce the release of Snap 0.5.3, a bugfix release of the Snap
Framework.

## New features

  - The [hint](http://hackage.haskell.org/package/hint) dynamic reloading
    feature is now optional in the
    [snap](http://hackage.haskell.org/package/snap) package; you can pass
    "`-f-hint`" to "`cabal install snap`" to disable hint support.


## Incompatible/breaking changes

  - The `snap-server` SSL backend has been changed from GnuTLS to OpenSSL. To
    build a `snap-server` with SSL support, you must now run `cabal install
    snap-server -fopenssl` instead of `-fgnutls`.
  
  - In
    [FileUploads.hs](https://github.com/snapframework/snap-core/blob/0.5.3/src/Snap/Util/FileUploads.hs),
    "maximumFormInputSize" has been changed from Int to Int64.


## Dependency changes

  - Several tweaks to dependencies to get Snap building on GHC 7.2; many thanks
    to Herbert Valerio Riedel and Bryan O'Sullivan for contributing these
    patches.
