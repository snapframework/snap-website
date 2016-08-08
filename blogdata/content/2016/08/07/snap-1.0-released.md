| title: Announcing: Snap 1.0
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2016-08-07T14:22:00-0400
| updated:   2016-08-07T14:22:00-0400
| summary: Release notes for Snap 1.0

The Snap team is delighted to announce the anxiously awaited release of version
1.0 of the Snap Web Framework for Haskell. Snap has been used in stable
production applications for years now, and with this release we're updating our
version number to reflect the stability and commitment to backwards
compatibility that our users depend on. Here is a summary of the major changes:

* Web server rewritten with the more efficient
  [io-streams](http://hackage.haskell.org/package/io-streams).

* [100% web server test
  coverage](https://snapframework.github.io/snap-code-coverage/snap-server/hpc-ghc-8.0.1/hpc_index.html)
  makes Snap the most robust and well tested Haskell web server.

* Better exception safety with
  [monad-control](http://hackage.haskell.org/package/monad-control).

* Continued strong commitment to backwards compatibility:
    - Very few breaking API changes
    - [Built and tested with the last 5 major GHC versions](https://travis-ci.org/snapframework/snap)

## The Details

### Now backed by io-streams

Snap's web server has been overhauled, replacing the enumerator package with the
newer, leaner, faster, and easier to use
[io-streams](http://hackage.haskell.org/package/io-streams). If you were using
of Snap's low-level enumerator functions, those will need to be migrated to
io-streams. Otherwise there should be few interface changes.

### More modular project template infrastructure

The snap executable that generates project templates has been moved from the
snap package to snap-templates. Your snap applications depending on snap will
continue to do so, but with a slightly lighter set of transitive dependencies.
If you want to run `snap init` to generate a project template, you will now need
to do `cabal install snap-templates` first instead of `cabal install snap`.

## Migration Guide

* Change your cabal files to depend on monad-control instead of
  MonadCatchIO-transformers.

* Instead of deriving the MonadCatchIO type class, you should now make
  MonadBaseControl instances. Depending on your monad, this may require
  MonadBase and MonadTransControl instances as well. For examples of how to do
  that for common monad structures, look at
  [Heist](https://github.com/snapframework/heist/blob/master/src/Heist/Internal/Types/HeistState.hs#L362)
  and snap
  ([here](https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Internal/Types.hs#L283),
  [here](https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Internal/Lensed.hs#L103),
  and
  [here](https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Internal/RST.hs#L114)).

* Any exception handling functions like `try`, `catch`, etc you were using
  from Control.Monad.CatchIO should now come from Control.Exception.Lifted
  which is provided by the lifted-base package.

* `initCookieSessionManager` takes an additional `Maybe ByteString` argument
  representing an optional cookie domain.  Passing Nothing as the new argument
  will give the same behavior as you had before.

## Outline

The Snap Framework is composed of five major packages:

* [snap-core](http://hackage.haskell.org/package/snap-core) - A simple and
  stable web server API.

* [snap-server](http://hackage.haskell.org/package/snap-server) - A robust and
  well tested web server implementing the snap-core API.

* [heist](http://hackage.haskell.org/package/heist) - An HTML 5 template system
  allowing designers to make changes to markup without needing to have a Haskell
  toolchain installed and recompile the app.

* [snap](http://hackage.haskell.org/package/snap) - Umbrella project that
  integrates the above three packages, provides a snaplet system for building
  reusable web components, and includes built-in snaplets for common things like
  sessions, auth, templating, etc.

* [snap-templates](http://hackage.haskell.org/package/snap-templates) - Provides
  an executable for generating Snap project templates.

## Acknowledgments

We would like to thank the dozens of contributors who have helped over the years
to get Snap to this milestone. Particular thanks go to Greg Hale who has been
instrumental in getting us across the finish line for this release.
