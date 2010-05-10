## Overview and Installation

The exposed Snap API is on the same level abstraction as Java Servlets. If you
understand servlets, most of the rest of the tutorial should be very
self-explanatory. Even if you don't, have no fear!  The following tutorial
only expects that you know Haskell.

Before we dive into writing our first Snap web application, let's do a quick
overview of the parts of the Snap framework. Currently Snap is divided into
three components:

- `snap-core` is the core of Snap. It includes type definitions and all code that
  is server-agnostic.
- `snap-server` is an HTTP server library built on top of `snap-core`. It
  currently includes a backend using stock Haskell socket I/O and a backend
  which uses the [libev](http://software.schmorp.de/pkg/libev.html) O(1)
  event loop library.
- `heist` is the X(HT)ML templating library. You do not need it to use Snap,
  but you are certainly welcome to.

To install Snap, simply use `cabal`. It's up to you whether or not you want to
use the Heist templating library. It is not required if you just want to use
the rest of Snap.

~~~~~~ {.shell}
$ cabal install snap-server
$ cabal install heist
~~~~~~

## Hello Snap

To generate a skeleton Snap web application, the `snap-core` package is
installs an executable `snap` into `$HOME/.cabal/bin`. We can use that to
create our "Hello Snap" application.

~~~~~~ {.shell}
$ mkdir hello-snap
$ cd hello-snap
$ snap init
~~~~~~

We now have a skeleton Snap project with a `.cabal` file and a source
directory. To see that it works, we can install it, point your browser to
`localhost:8000` and see that just prints "hello world". (If you did not
install Heist, you need to remove the `import Text.Templating.Heist` line in
`Main.hs`.)

~~~~~~ {.shell}
$ cabal install
$ hello-snap 8000 &
$ curl localhost:8000
~~~~~~

When you are satisfied, we can kill the server.

~~~~~~ {.shell}
$ fg
hello-snap 8000
^C
~~~~~~

We'll be extending our toy application to do more useless things, like echoing
a part of the request URL.


### The `Snap` Monad

All our actions that we want the web application to perform are written inside
the `Snap` monad. Basically, the `Snap` monad is a state transformer that lugs
around a request from the server and a response that it should give back to the
server. The programmer (that's you) modifies the response according to the
request, and after your web handler finishes, the server writes the response
back to the end-user over HTTP.

The reading of requests is standard fare. You can get the request out of
`Snap` monad by calling the function `getRequest`. For the specific functions
on getting fields out of the request, please refer to the [API
documentation](/docs/latest/snap-core/index.html).

The writing of certain parts of responses is also standard fare. Like for
requests, you can get the response by calling `getResponse`. You can also put a
new response into the state with `putResponse`, and modify the existing one
with `modifyResponse`. These are also documented in the [API
documentation](/docs/latest/snap-core/index.html).

Writing the response body, however, is a little tricky and requires some
explanation of things called "iteratees" and "enumerators", which we will get
to in a bit.

The `Snap` monad provides type class instances for `MonadPlus` and
`Alternative`. For those unfamiliar with these Haskell type classes, we
recommend taking a look at Brent Yorgey's outstanding
[Typeclassopedia](http://haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf).
Having an `Alternative` instance means that `Snap` monad actions can "fail";
here failure translates to "we decided not to handle this request, and you can
skip further processing". The `empty` or `mzero` values in the `Snap` monad
cause the computation to fail -- for any `a`, `empty >> a --> empty`.

Given a failure mode, we can combine actions in the `Snap` monad in an
intuitive way using the `<|>` infix operator. Two actions `<|>`'d together
means something like "try the first action, and if it fails, then do the second
one".

Before we continue, let us explain iteratees and enumerators.  You do not need
to read the intermezzo to understand the rest of the tutorial.  If you choose
to skip it, the take-home lesson is that the convenience functions like
`writeBS` do not immediately write out to the socket. Instead, you are
composing functions behind the scenes into one big function that will write
your output out at the end of the computation.


#### Intermezzo: Iteratee I/O

The Snap framework uses a style of I/O called "iteratee I/O". We have opted for
iteratees over handle-based and lazy I/O because iteratee I/O offers better
resource management, error reporting, and composability.  Iteratees underlie
the entire system, so if you want to use Snap proficiently it certainly helps
to understand iteratees. The following explanation is cursory at best, but we
hope the analogies help.

So what are iteratees? Iteratees are things (functions) that are "iterated
over" a stream. I like to compare iteratees to the video game character Kirby,
who changes his state depending on what he consumes. Iteratees are kind of
like that. You feed an iteratee some input from the stream, it does something
with it, and the iteratee is either "done" and gives you back some computed
value, refusing to consume anymore, or gives you back _another_ iteratee
that's ready for more input (changes into another iteratee, if you will). That
is, iteratees consume data a chunk at a time, and if it's expecting more, it
encodes the intermediate state of the computation using all that closure
goodness into a continuation iteratee.

Iteratees are the consumers of data, and their incremental nature
unsurprisingly gives us incremental processing. This is a very useful property
to have in an HTTP server! In the Snap framework, we fix the "stream" to be
strict `ByteString`s, and iteratees are used to consume many kinds of
`ByteString`s. They can take a raw HTTP request and give back a parsed result.
They are also the ones that take your response body, the stuff that you told
the `Snap` monad to write out using `writeBS` and the like, and sends it over
the socket.

That's the story for iteratees, but who's in charge of the caring and feeding
the iteratees? Those are the enumerators.  Enumerators take an iteratee `f` and
returns another iteratee, which when "run" (by feeding it with EOF, because
iteratees are forced to go into the "Done" state when they receive EOF), causes
a series of data chunks to be written to `f`. These functions are the producers
of data, they take some iteratee as input and keep feeding it data until the
iteratee says it is done or the enumerator runs of data to feed the
iteratee. In this sense enumerators may be more naturally thought of as
_iteratee transformers_.

Since we are Haskell programmers, it is not surprising that iteratees and
enumerators are monads and compose nicely (using the "`>.` operator). In Snap,
enumerators are prominently used for the response body. When you write
`writeBS`, we are actually making an enumerator that will feed your string to
the output iteratee, and composing it with the enumerator that was already in
the response. In other words:

~~~~~~ {.haskell}
foo :: Snap ()
foo = do
    writeBS "foo"
    writeBS "bar"
    writeBS "baz"
~~~~~~

Is the same as

~~~~~~ {.haskell}
foo :: Snap ()
foo = modifyResponse $ setResponseBody $
      (enumBS "foo" >. enumBS "bar" >. enumBS "baz")
~~~~~~

This is kind of weird for people who are not used to it because it's an
inversion of control. You as the programmer are not actually sending anything
out to a socket; you're actually assembling a "program", that when given an
iteratee, feeds that data to the iteratee. What actually happens is that we
give the response body enumerator an iteratee that sends data out over the
socket, potentially with a `transfer-encoding` applied.

For other convenience functions that manipulate the response body enumerator,
consult the [API documentation](/docs/latest/snap-core/index.html).

We hope this quick and dirty introduction to iteratee I/O has shed some light
on using Snap. For further discussion, the original talk and other iteratee
resources may be found on [Oleg's site](http://okmij.org/ftp/Streams.html), the
[iteratee](http://hackage.haskell.org/package/iteratee) package API docs, and
the [Haskell Wiki](http://www.haskell.org/haskellwiki/Iteratee).

### Routing

Our goal is to get our toy application to spit back a portion of the request
URL. That is, I want the following behavior:

~~~~~~ {.shell}
$ curl localhost:8000/echo/ohsnap; echo
ohsnap
~~~~~~

So first, we need to tackle how to route URLs. Snap provides two types of
routing functions: combinators and `route`. Some combinators are as follows:

- `<|> :: Snap a -> Snap a -> Snap a`
- `ifTop :: Snap a -> Snap a`
- `dir :: ByteString -> Snap a -> Snap a`
- `method :: Method -> Snap a -> Snap a`

These do exactly what you might think they do. Calling `ifTop`:

~~~~~~ {.haskell}
foo :: Snap ()
foo = ifTop $ do ...
~~~~~~

takes an action in the Snap monad and transforms it to only run if the request
path is at the "top level" (specifically, when `rqPathInfo` is empty). The
`dir` combinator transforms the action to run if the request path starts with
the specified directory. The `method` combinator matches on specific HTTP
methods.

The astute reader will no doubt now see that these functions can be used in
conjunction with `Alternative` semantics of the `Snap` monad to do routing.
However, this kind of routing is expensive in that it can be O(n) in the number
of handlers. To remedy this, Snap also provides you with a function that routes
requests based on the request path in `O(log n)` time:

- `route :: [(ByteString, Snap a)] -> Snap a`

This function takes an associative list of paths and actions and combines it
all together into one action that does the routing. It also supports capturing
parts of the path into parameters.

Let's see this in action to route the path `/echo` to some echo action. Open
up `Main.hs` in your favorite editor and edit `site` to look like the
following.

~~~~~~~~~~~~~~~ {.haskell}
site :: Snap ()
site = route [ ("", ifTop (writeBS "hello world"))
             , ("echo/:s", echoHandler) ] <|> fileServe "."
~~~~~~~~~~~~~~~

The above code routes the top level empty path to the original "hello world"
action, and the path `echo/` to `echoHandler` (which we haven't written yet).
More importantly, when it routes `echo`, it captures the next immediate path
component (not the rest of the path) into a parameter named `s`. We can access
`s`, or any other request parameter, with the function `rqParam`. If the
request was neither to the top level nor to `echo`, we try to find a file in
the current directory with the same name as the request path and serve that.

Moving on, let's write `echoHandler`, which should just spit back `s`.

FIXME: non-exhaustive pattern match; should have a convenience function
`getParam :: ByteString -> Snap (Maybe ByteString)` and `getParams ::
ByteString -> Snap [ByteString]`.

~~~~~~~~~~~~~~~ {.haskell}
echoHandler :: Snap ()
echoHandler = do
    req <- getRequest
    case (rqParam "s" req) of
        Just [s] -> writeBS s
        _        -> writeBS ""
~~~~~~~~~~~~~~~

Let's build our toy application and run it again. Now we have a perfectly
useless server that echos paths that start with `echo`!

~~~~~~ {.shell}
$ curl localhost:8000/echo/foo; echo
foo
~~~~~~


### Error Handling

Pretend that we wrote this echo server for an elementary school, and the PTA
has requested that profanities be censored and unechoable. The parents of PTA
also happen to be not very bright and have taken the euphemism "4-letter word"
too literally. They have requested that we block all 4-letter words. For
arbitrary reasons, we have decided to respond to echo requests of 4-letter
words by sending a `403 Forbidden` error.

We'll need `ByteString` utility functions for this, so be sure to add the
following line somewhere in your list of `import`s.

~~~~~~~~~~~~~~~ {.haskell}
import qualified Data.ByteString as B
~~~~~~~~~~~~~~~

Let's rewrite our `echoHandler` as follows.

~~~~~~~~~~~~~~~ {.haskell}
echoHandler :: Snap ()
echoHandler = do
    req <- getRequest
    case (rqParam "s" req) of
        Just [s] -> if (B.length s == 4) then badWord else (writeBS s)
        _        -> writeBS "bad input!"
  where
    badWord = do
        putResponse $
          setResponseStatus 403 "Forbidden" emptyResponse
        writeBS "Forbidden: four-letter word"
        r <- getResponse
        finishWith r
~~~~~~~~~~~~~~~

The `badWord` action sets the response status code to be 403 using a
combination of the `putResponse` and `setResponseStatus` functions, writes out
the body of the response, and calls the function `finishWith` on that response.

The `finishWith` combinator is a function you can call to short-circuit the
monad processing so that no further `Snap` actions will be run; use it when
you're really, really sure about what you want to return and you don't want
your response to be mucked with.

Let's test this with some childish language to see that it works.

~~~~~~ {.shell}
$ curl -i localhost:8000/echo/poop
HTTP/1.1 403 Forbidden
Date: Mon, 10 May 2010 07:08:32 GMT
Server: Snap/0.pre-1
Transfer-Encoding: chunked

Forbidden: four-letter word
~~~~~~

## What Now?

We hope we've whet your appetite for using Snap. From here on out you should
take a look at the [API documentation](/docs/latest/snap-core/index.html),
which explains many of the concepts and functions here in further detail.

You can also come hang out in `#snap-framework` on `freenode`.
