| title: Announcing: Snap Framework v0.5.5
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-10-16 12:13:00+0200
| updated: 2011-10-16 12:13:00+0200
| summary: Release notes for Snap 0.5.5

The Snap team is happy to announce the release of version 0.5.5 of the Snap
Framework. Here are the changes since 0.5.3:

## Bugfixes

  - The file upload code now uses something better than
    [`openBinaryTempFile`](http://haskell.org/ghc/docs/latest/html/libraries/base/System-IO.html#v:openBinaryTempFile),
    which uses a spectacularly dumb algorithm.

  - We now properly clean up uploaded temporary files when the read end of the
    socket dies. The root problem is that the
    [enumerator](http://hackage.haskell.org/package/enumerator) library
    conceptual model doesn't allow "`bracket`" for resources opened by
    iteratees, since the enumerator can simply choose not to inform the
    iteratee of an error on the read end. Our current hacky workaround is to
    use a GC finalizer; we'll be fixing this properly for Snap 0.7.

  - [`Snap.Util.FileServe`](http://hackage.haskell.org/packages/archive/snap-core/0.5.4/doc/html/Snap-Util-FileServe.html):
    "`serveDirectoryWith fancyDirectoryConfig`" now properly prefixes pathnames
    in directory listings.

  - [`Snap.Util.FileServe`](http://hackage.haskell.org/packages/archive/snap-core/0.5.4/doc/html/Snap-Util-FileServe.html):
    `fancyDirectoryConfig` now correctly handles unicode filenames.

  - Fixed the haddock comment for
    [`Snap.Util.Readable`](http://hackage.haskell.org/packages/archive/snap-core/0.5.4/doc/html/Snap-Util-Readable.html).
    
  - Fixed [issue #79](https://github.com/snapframework/snap-core/issues/79), in
    which `snap-server` would leak memory if log files were not writable.
    
  - We now log more information in exception handlers. See
    [issue #78](https://github.com/snapframework/snap-core/issues/78).

  - Fix `--no-access-log` and `--no-error-log` options in
    [`commandLineConfig`](http://hackage.haskell.org/packages/archive/snap-server/0.5.4/doc/html/Snap-Http-Server-Config.html).
    
  - Fixed a stupid bug in which activity on the write end of the socket was not
    extended the timeout as it should. Activity on the write end of the socket
    also now only *extends* the timeout instead of potentially shortening it.
    
  - We now call `withSocketsDo` inside `snap-server` to initialize the network
    on Windows.
    
  - Access to stderr in the logging code is now serialized. When two loggers
    started up at once, and they both could not open their output file, the
    error output written to stderr was garbled. Access to stderr is now
    serialized with a lock.


## New features / improvements

  - The FileServe module now has a MIME type entry for JSON.

  - We've converted
    [`Snap.Util.GZip`](http://hackage.haskell.org/packages/archive/snap-core/0.5.4/doc/html/Snap-Util-GZip.html)
    to use the [zlib-enum](http://hackage.haskell.org/package/zlib-enum)
    package.

  - Reordered constructors in the `Snap` monad in order of frequency, which
    should improve generated code.

  - Added a
    [`MonadError String`](http://hackage.haskell.org/packages/archive/mtl/2.0.1.0/doc/html/Control-Monad-Error.html)
    instance to the Snap monad.
    
  - Added a `noCompression` function to
    [`Snap.Util.GZip`](http://hackage.haskell.org/packages/archive/snap-core/0.5.4/doc/html/Snap-Util-GZip.html).
    
  - Added IPv6 support to snap-server.


## Dependency changes

  - Snap should once again build with GHC 6.12.
