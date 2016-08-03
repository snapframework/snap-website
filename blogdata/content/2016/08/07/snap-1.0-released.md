| title: Announcing: Snap 1.0
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2016-08-07T14:22:00-0400
| updated:   2016-08-07T14:22:00-0400
| summary: Release notes for Snap 1.0

The Snap team is pleased to announce the release of version 1.0 of the Snap
Web Framework for Haskell.

## Major Changes

### Now backed by io-streams

### Exception handling with monad-control

* Change your cabal files to depend on monad-control instead of
  MonadCatchIO-transformers.

* Instead of deriving the MonadCatchIO type class, you should now make
  MonadBaseControl instances.  Depending on your monad, this may require
  MonadBase and MonadTransControl instances as well.  For examples of how to
  do that for common monad structures, grep the snap and heist packages for
  MonadBaseControl.

* Any exception handling functions like `try`, `catch`, etc you were using
  from Control.Monad.CatchIO should now come from Control.Exception.Lifted
  which is provided by the lifted-base package.

### Other Changes

* `initCookieSessionManager` takes an additional `Maybe ByteString` argument
  representing an optional cookie domain.  Passing Nothing as the new argument
  will give the same behavior as you had before.

## Minor improvements

