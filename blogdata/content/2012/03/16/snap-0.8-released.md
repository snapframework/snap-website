| title: Announcing: Snap Framework v0.8
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2012-03-16T20:00:00+0100
| updated:   2012-03-16T20:00:00+0100
| summary: Release notes for Snap 0.8

The Snap team would like to announce the release of version 0.8 of the Snap
Framework.


## New features / improvements

### Running behind proxy servers

Added a new `Snap.Util.Proxy` module that will rewrite the `rqRemoteAddr` field
in an incoming `Request` based on parsing the value of the `X-Forwarded-For`
header. Support for this has also been integrated into `snap-server`'s basic
command-line argument processing, so, assuming your proxy server uses the
standard `X-Forwarded-For` header, you can tell `snap-server`-enabled
applications to do proxy address translation by running the binary with the
"`--proxy=X_Forwarded_For`" flag.


### Timeout handling

Timeout handling has been tweaked slightly in Snap 0.8; the Snap monad
contained a function to return an action to *set* the timeout:

~~~~~~~~~~ {.haskell}
getTimeoutAction :: MonadSnap m => m (Int -> IO ())
~~~~~~~~~~

Several users, however, wanted support for timer *extension* semantics. Snap
now exports a new function:

~~~~~~~~~~ {.haskell}
getTimeoutModifier :: MonadSnap m => m ((Int -> Int) -> IO ())
~~~~~~~~~~

Accordingly, there are new convenience functions to go along with `setTimeout`:

~~~~~~~~~~ {.haskell}
extendTimeout :: MonadSnap m => Int -> m ()
modifyTimeout :: MonadSnap m => (Int -> Int) -> m ()
~~~~~~~~~~

Note that this has also caused a breaking change to the `escapeHttp` function,
which hopefully only Jasper should care about :-)


### Separate maps for form parameters and query parameters

A bug from the stone age, Snap now separates out form parameters from query
parameters. `Request` objects now contain `rqQueryParams` and `rqPostParams`
`Params` objects, with the old `rqParams` now being the union of the two. Both
`rqQueryParams` and `rqPostParams` are immutable.


### Improvements to `Snap.Test`

The `Snap.Test` test-harness module was updated with some new functionality:

  - `RequestBuilder` wasn't setting `rqContextPath` properly -- the docs say
    that `rqContextPath` must begin and end with a slash.

  - Exposed the `runHandlerM` function to run a Snap handler from an arbitrary
    monad transformer over `Snap`.

  - Added new `evalHandler` and `evalHandlerM` functions to get the monadic
    value produced by a `Snap` action.


### Improvement to the stock 404 page

The fallthrough 404 page (which users should always override for their own
sites) has been extended to report the `rqURI` for the request.


### Changes to buffering semantics

For various reasons (mostly due to shortcomings in the various Haskell `zlib`
libraries that, up until recently, made it difficult to pass `Z_SYNC_FLUSH`
through to the `zlib` library), Snap would sometimes cause certain kinds
long-polling/interactive applications to stall because there was no reliable
way to indicate that you wanted to flush the output buffer.

While that's still the case, Michael Snoyman has
[fixed the underlying issue in `zlib-bindings`](https://github.com/snoyberg/zlib-bindings/commit/3a4bc720c5bd694baf4a346a3369d62335e46c7c),
the functionality just has to be exposed through `zlib-enum`. Until then, we've
added a field to `Response` allowing users to turn off buffering altogether,
and added some functions to modify it:

~~~~~~~~~~ {.haskell}
getBufferingMode :: Response -> Bool
setBufferingMode :: Bool        -- ^ if True, buffer the output, if False, send
                                -- output immediately
                 -> Response
                 -> Response
~~~~~~~~~~

Obviously, removing buffering might kill performance, so it's only recommended
to use this if you're doing something like
[Comet](http://en.wikipedia.org/wiki/Comet_%28programming%29) where you care
about sending some output immediately but holding the output open to write more
later.


### Turn TCP_NODELAY on for all of our sockets

We do our own buffering, so the Nagle algorithm only serves to potenially delay
our packets.


### Snaplets: add bracketHandler function

Added the following function to `snap`:

~~~~~~~~~~ {.haskell}
bracketHandler :: IO a -> (a -> IO x) -> (a -> Handler b v c) -> Handler b v c
~~~~~~~~~~


### Heist: expose evalHeistT

Heist 0.8.0 now exposes `evalHeistT`:

~~~~~~~~~~ {.haskell}
evalHeistT :: Monad m => HeistT m a -> X.Node -> HeistState m -> m a
~~~~~~~~~~


### Heist: support for binding JSON values

Added the `bindJson` splice:

~~~~~~~~~~ {.haskell}
------------------------------------------------------------------------------
-- | This splice binds convenience tags for the given JSON (or
-- JSON-convertible) value and runs the tag's child nodes using the new
-- bindings.
--
-- /Tags bound when you pass in an object/
--
-- Tags bound for an object looking like this:
--
-- > { "k_1": v_1, ..., "k_N": v_N }
--
-- @\<value:{k_i}\>@    -- treats v_i as text
-- @\<snippet:{k_i}\>@  -- treats v_i as HTML
-- @\<with:{k_i}\>@     -- explodes v_i and runs its children
--
-- @\<value var=\"foo.bar.baz\"\/>@ -- walks the JSON tree to find
-- \"foo.bar.baz\", and interprets it as a string
-- @\<snippet var=\"foo.bar.baz\"\/\>@
-- @\<with var=\"foo.bar.baz\"\>...\<with\>@
--
-- /Tags bound when you pass in anything else/
--
-- @\<value\/\>@    --  the given JSON value, as a string
-- @\<snippet\/\>@  --  the given JSON value, parsed and spliced in as HTML
--
bindJson :: (ToJSON a, Monad m) => a -> Splice m
~~~~~~~~~~

This should make it easy to use JSON values (or JSON-convertible) in heist
templates.


## Bugfixes

### Route capture

Capture variables in routes no longer capture the empty string. Formerly, the
following code would match the input url "`/foo/`":

~~~~~~~~~~ {.haskell}
handler :: Snap ()
handler = route [ ("foo/:id", fooHandler) ]
~~~~~~~~~~

Users found that this violated the principle of least surprise (especially
since the capture handler might supercede the handler for "`foo/`"), so the
semantics have been changed.


### File serving: fix links in directory indices

Fixed [bug #121](https://github.com/snapframework/snap-core/issues/121), in
which Snap was emitting wrong links in a directory index under some
circumstances.


### Fix the way snap intercalates headers

The HTTP standard states that if the same header appears in a request or
response multiple times, they may be treated as if they were one header
intercalated with a comma. We were improperly intercalating with a space
before.


### Libev backend: setting a short timeout now works

Fixed a bug in `snap-server`'s libev backend, in which timeouts could only be
extended, never shortened.


### SSL bug re: cleaning up after initialization

We were not correctly cleaning up the socket if SSL handshaking failed. Also,
changed from bidirectional OpenSSL shutdown to unidirectional.


### Fix bug in http header size limiting

Snap will now read no more than 256kB of HTTP headers information.


### Snaplets no longer depend on the value of $CWD after initialization

Snaplets were relying on the value of the current working directory after
initialization (most notably on reload) -- snaplets now store fully-qualified
pathnames internally.


### Fix embedSnaplet routing bug

Fixed a bug in embedSnaplet that caused routes to be prepended with one too
many prefixes.


### Snaplet initialization cleanup

Exceptions thrown by Snaplet initializers are now caught properly, and unload
actions that were registered before the exception was thrown are now executed.

### Heist snaplet filepath change

The Heist snaplet used to look for templates in the user-specified directory
relative to the project root.  This yielded incorrect behavior when using the
snaplet somewhere other than at the top level.  Now the Heist snaplet looks in
it's correct root in the filesystem: snaplets/heist.  If your application has
this code

~~~~~~~~~~ {.haskell}
hs <- nestSnaplet "" heist $ heistInit "templates"
~~~~~~~~~~

...in 0.7 your templates would have been in templates.  In 0.8 they should
reside in snaplets/heist/templates.

### New semantics for addTemplatesAt

The URLs generated by addTemplatesAt are now (correctly) relative to the
snaplet's root URL.  If you use this function or the addTemplates function to
manually add templates that were not loaded by heistInit, you will have to
change your code.

### Heist: fix small bug with $-expansion in attributes

Fixed [Heist bug #16](https://github.com/snapframework/heist/issues/16), in
which a bare "$" in an html attribute would incorrectly trigger variable
expansion.


### Heist: fix stack overflow when using attribute substitutions

Fixed [Heist bug #13](https://github.com/snapframework/heist/issues/13), in
which using an attribute substitution inside an apply tag led to a stack
overflow.


## Dependency changes

Snap has been updated to the newest version of too many dependencies to count.
