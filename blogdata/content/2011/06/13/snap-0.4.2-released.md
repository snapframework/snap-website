| title: Announcing: Snap Framework v0.4.2
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-06-14T13:56:00-0400
| updated: 2011-06-14T13:56:00-0400
| summary: Release notes for Snap 0.4.2

The Snap team is proud to (belatedly) announce the release of Snap 0.4.2,
containing bugfixes and efficiency improvements. Snap 0.4.2 was released to
Hackage a couple of weeks ago; apologies for just getting to the release notes
now. Here are the changes since 0.4.1:

## Bugfixes

  - Fixed a small bug in file upload support re: rate limiting.

  - Fixed another bug in the `MonadCatchIO` instance for `Iteratee`; it was
    gobbling unconsumed input when it didn't have to.

  - Fixed [issue #68](https://github.com/snapframework/snap-core/issues/68),
    "Connections cannot be reused if requests specify Content-Length: 0".

  - Fixed a spec-compliance bug: send error code 411 when PUT/POST requests
    come in with no content-length.

  - Explicitly link against `-lgcrypt` when using `gnutls` -- this was causing
    link failures on some platforms.

  - We now log exceptions caught by the default error handler.


## Functions added

  - Added a function to `Snap.Types`:

~~~~~~ {.haskell}
methods :: MonadSnap m => [Method] -> m a -> m a
~~~~~~


## Efficiency improvements

  - In our
    [Boyer-Moore-Horspool](http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm)
    implementation, achieved some efficiency improvements by eliminating some
    bytestring bounds checking and killing an unnecessary copy.

  - Improved timeout efficiency on the `snap-server` "simple" backend using a
    technique inspired by some conversations with Michael Snoyman (thanks,
    Michael!)

  - Replaced our Attoparsec HTTP parser with a hand-rolled one; makes parsing
    slightly more efficient. Please report any problems.


## Dependencies

  - Relaxed the upper bound on the
    [blaze-builder](http://hackage.haskell.org/package/blaze-builder)
    dependency to match the latest version.

  - Bumped the lower bound on the
    [bytestring-mmap](http://hackage.haskell.org/package/bytestring-mmap)
    dependency; older bytestring-mmap versions were causing compile failures on
    GHC 7.


## Other improvements

  - Increased test coverage for `snap-core`.
