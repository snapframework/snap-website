| title: Announcing: Snap Framework v0.3
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-12-20T19:15:00+0100
| updated: 2010-12-20T19:15:00+0100
| summary: Release notes for Snap 0.3, a new major version of the Snap Framework.

After many months of hard work, the Snap team is proud to announce the release
of the next major version of the Snap Framework: Snap 0.3.

## New Features

  - Snap now has SSL support! To enable SSL support, install the
    [gnutls](http://www.gnu.org/software/gnutls/) library and pass the
    "`gnutls`" in when you build `snap-server`:

~~~~~~~~~ {.shell}
$ cabal install snap-server -fgnutls
~~~~~~~~~

  - The `snap` project starter executable has been pulled out of `snap-core`
    and put into its own package, called
    "[snap](http://github.com/snapframework/snap/)". The `snap` package also
    contains some library code which the generated projects make use of, and
    the generated projects have a simplified and much cleaner layout.

  - Snap now comes with support for working in a "development mode" which runs
    your web handlers in an interpreter using the
    [hint](http://hackage.haskell.org/package/hint) library. This makes working
    on Snap applications quicker and more convenient, since you no longer need
    to perform a kill/recompile/reload cycle to see your code changes reflected
    in your running application.
    
  - One of the new things in the `snap` package is an experimental "extensions"
    mechanism which contains some interfaces/helper code for writing reusable
    web components.

  - A new `MonadSnap` typeclass has been defined:
    
    ~~~~~~~~~ {.haskell}
    class (Monad m, MonadIO m, MonadCatchIO m, MonadPlus m, Functor m,
           Applicative m, Alternative m) => MonadSnap m where
        liftSnap :: Snap a -> m a
    ~~~~~~~~~
    
    Most of the basic functions from `Snap.Types` have been redefined to accept
    any monad implementing `MonadSnap`, and `MonadSnap` instances have been
    defined for most of the standard monad transformers; this should make it
    much more convenient to use monad transformers with Snap without having to
    use "`lift`" all over the place.

  - Added `getCookie :: MonadSnap m => ByteString -> m (Maybe Cookie)`, which
    allows you to fetch cookies from the request by name.

  - Snap now depends on the `enumerator` package for iteratee support instead
    of `iteratee`.

  - The `snap-server` now has a convenient configuration mechanism which will
    parse arguments from the command line for you.


## Bugfixes/Improvements

  - Changed type signature of `finishWith` from `Response -> Snap ()` to 
    `Response -> Snap a`.

  - When using `fileServe`, file paths are now properly URL-decoded, meaning
    you can now download a file with spaces in its name.

  - Parameters grabbed by variable capture using `route` are also now
    URL-decoded.

  - Under the hood: snap server backend mechanism has been improved/factored
    out, making it possible to listen to two or more ports using different
    backends at once (e.g. SSL and non-SSL).
