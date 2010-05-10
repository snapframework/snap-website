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
- `snap-server` is the HTTP server. It currently includes a simple backend and a
  `libev` backend.
- `heist` is the X(HT)ML templating library. You do not need it to use Snap,
  but you are certainly welcome to.

To install Snap, simply use `cabal`. It's up to you whether or not you want to
use the Heist templating library. It is not required to use the rest of Snap.

	$ cabal install snap-server
	$ cabal install heist

## Hello Snap

To generate a skeleton Snap web application, the `snap-core` package is
installs an executable `snap` into `$HOME/.cabal/bin`. We can use that to
create our "Hello Snap" application.

	$ mkdir hello-snap
	$ cd hello-snap
	$ snap init

We now have a skeleton Snap project with a `.cabal` file and a source
directory. To see that it works, we can install it, point your browser to
`localhost:8000`, and see that it prints "hello world". (If you did not
install Heist, you need to remove the `import Text.Templating.Heist` line in
`Main.hs`.)

	$ cabal install
	$ hello-snap 8000 &
	$ curl localhost:8000

When you are satisfied, we can kill the server.

	$ fg
	hello-snap 8000
	^C

Let's extend our toy application to do more useless things, like echoing a
part of the request URL.

### The `Snap` Monad

All the actions that we want the web application to perform are written inside
the `Snap` monad. Basically, the `Snap` monad is a state transformer that lugs
around a request from the server and a response that it should give back to
the server. The programmer (that's you) modifies the response according to the
request, and in the end the server writes the response back to the end-user
over HTTP.

You can get the request out of `Snap` monad by calling the function
`getRequest`. For the specific functions on getting fields out of the
request, please refer to the [API
documentation](/docs/latest/snap-core/index.html).

As with requests, you can get the response by calling `getResponse`.
You can also put a new response with `putResponse` and modify the
existing one with `modifyResponse`. These are also documented in the
[API documentation](/docs/latest/snap-core/index.html).

The `Snap` monad provides type class instances for `MonadPlus` and
`Alternative`. For those unfamiliar with these Haskell type classes, we
recommend taking a look at Brent Yorgey's outstanding
[Typeclassopedia](http://haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf).

Practically, what this means it that we are able to combine actions in the
`Snap` monad in an intuitive way using `<|>` and `empty`. Two actions
`<|>`'d together means something like "do the first action, if it fails, then
do the second one". `empty` is the default failure action that does nothing.

Before we continue, let us explain iteratees and enumerators.  You do
not need to read the following intermezzo to understand the rest of
the tutorial.  If you choose to skip it, the take-home lesson is that
the convenience functions like `writeBS` do not immediately write out
to the socket. Instead, you are composing functions behind the scenes
into one big function that will write your output out at the end of
the computation.

#### Intermezzo: Iteratee I/O

The Snap framework uses a style of I/O called "iteratee I/O". We have
opted for its use over handle-based and lazy I/O because iteratee I/O
offers better resource management, error reporting, composability, and
speed.  Iteratees underlie the entire system, so if you want to use
Snap proficiently it certainly helps to understand iteratees. The
following explanation is cursory at best, but we hope the analogies
help.

So what are iteratees? Iteratees are things (functions) that are
"iterated over" a stream. I like to compare iteratees to the video
game character Kirby, who changes his state depending on what he
consumes. Iteratees are kind of like that. You feed an iteratee some
input from the stream, it does something with it, and the iteratee is
either "done" and gives you back some computed value, refusing to
consume anymore, or gives you back _another_ iteratee that's ready for
more input (changes into another iteratee, if you will). That is, an
iteratee consumes data a chunk at a time, and if it's expecting more,
it encodes the intermediate state of the computation using all that
closure goodness into a continuation iteratee.

Iteratees are the consumers of data, and their incremental nature
unsurprisingly gives us incremental processing. This is a very useful
property to have in an HTTP server! In the Snap framework, we fix the
"stream" to be strict `ByteString`s, and iteratees are used to consume
many kinds of `ByteString`s. They can take a raw HTTP request and give
back a parsed result.  They are also the ones that send your response
body, the stuff that you told the `Snap` monad to write out using
`writeBS` and the like, over the socket.

That's the story for iteratees, but who's in charge of the caring and feeding
the iteratees? Those are the enumerators.  Enumerators take an iteratee and
return another iteratee. These functions are the producers of data, they take
some iteratee as input and keep feeding it data until the iteratee says it is
done or the enumerator runs of data to feed the iteratee. The enumerator then
returns the resulting iteratee. In this sense enumerators may be more
naturally thought of as _iteratee transformers_.

Since we are Haskell programmers, it is not surprising that iteratees and
enumerators are monads and compose nicely. In Snap, enumerators are
prominently used for the response body. When you write `writeBS`, we are
actually making an enumerator that will feed whatever iteratee given to it the
`ByteString` that was passed in. This is kind of weird for people who are not
usde to it because it inverts control. You as the programmer are not actually
sending anything out to a socket, but are actually assembling a program, that
when given an iteratee, feeds that data to the iteratee. It so happens that we
give the response body enumerator an iteratee that sends out the data it's fed
over the socket. Moreover, owing to the incredible ease of composing
enumerators, `writeBS` is composed with the existing response body.  The
meaning of this composition is intuitive. Using the composition operator `>.`,
`(writeBS "foo") >. (writeBS "bar")` first feeds its iteratee "foo", and then
feeds the returned iteratee "bar".

For other convenience functions that manipulate the response body enumerator,
consult the [API documentation](/docs/latest/snap-core/index.html).

We hope this quick and dirty introduction to iteratee I/O has shed some light
on using Snap. For further discussion, the original talk and other iteratee
resources may be found on [Oleg Kiselyov's
page](http://okmij.org/ftp/Streams.html).

### Routing

Our goal is to get our toy application to spit back a portion of the request
URL. That is, I want the following behavior:

	$ curl localhost:8000/echo/ohsnap
	ohsnap$

So first, we need to tackle how to route URLs. Snap provides two types of
routing functions: combinators and `route`. Some combinators are as follows:

- `ifTop :: Snap a -> Snap a`
- `dir :: ByteString -> Snap a -> Snap a`
- `method :: Method -> Snap a -> Snap a`

These do exactly what you think they do. `ifTop` takes an action in the Snap
monad and transforms it to only run if the request path is at the "top level"
(specifically, when `rqPathInfo` is empty). `dir` transforms the action to run
if the request path starts with the specified directory. `method` matches on
specific HTTP methods (GET, POST, etc).

The astute reader will no doubt now see that these functions can be used in
conjunction with `Alternative` semantics of the `Snap` monad to do routing.
However, this kind of routing is expensive in that it requires exhaustive
search in O(n)! To remedy this, Snap also provides you with a function that
routes requests based on the path in O(log n):

- `route :: [(ByteString, Snap a)] -> Snap a`

This function takes an associative list of paths and actions and combines it
all together into one action that does the routing. It also supports capturing
parts of the path into parameters.

Let's see this in action to route the path `/echo` to some echo action. Open
up `Main.hs` in your favorite editor and edit `site` to look like the
following.

~~~~~~~~~~~~~~~ {.hs}
site :: Snap ()
site = route [ ("", ifTop (writeBS "hello world"))
             , ("echo/:s", echoHandler) ] <|> fileServe "."
~~~~~~~~~~~~~~~

The above code routes the top level empty path to the original "hello world"
action, and the path `echo/` to `echoHandler` (which we haven't written yet).
More importantly, when it routes `echo`, it captures the next immediate
component (not the rest of the path) into a parameter named `s`. We can access
`s`, or any other request parameter, with the function `rqParam`. If the
request was neither to the top level nor to `echo`, we try to find a file in
the current directory with the same name as the request path and serve that.

Moving on, let's write `echoHandler`, which should just spit back `s`.

~~~~~~~~~~~~~~~ {.hs}
echoHandler :: Snap ()
echoHandler = do
    req <- getRequest
    writeBS $ maybe "" id (rqParam "s" req)
~~~~~~~~~~~~~~~

Let's build our toy application and run it again. Now we have a perfectly
useless server that echos paths that start with `echo`.

	$ curl localhost:8000/echo/foo
	foo$

### Error Handling

Pretend that we wrote this echo server for an elementary school, and the PTA
has requested that profanities be censored and unechoable. The parents of PTA
also happen to be not very bright and have taken the euphemism "4-letter word" too
literally. They have requested that we block all 4-letter words. For arbitrary
reasons, we have decided to respond to echo requests of 4-letter words by
sending a `500`.

We'll need `ByteString` utility functions for this, so be sure to add the
following line somewhere in your list of `import`s.

~~~~~~~~~~~~~~~ {.hs}
import qualified Data.ByteString as B
~~~~~~~~~~~~~~~

Let's rewrite our `echoHandler` as follows.

~~~~~~~~~~~~~~~ {.hs}
echoHandler :: Snap ()
echoHandler = do
    req <- getRequest
    case (rqParam "s" req) of
        Just [s] -> if (B.length s == 4) then badWord else (writeBS s)
        _        -> writeBS "bad input!"
  where
    badWord = do
        modifyResponse $ setResponseStatus 500 "Internal Server Error"
        writeBS "500"
        r <- getResponse
        finishWith r
~~~~~~~~~~~~~~~

In prose, the `badWord` action modifies the response status code to be 500
using a combination of the `modifyResponse` and `setResponseStatus` functions,
writes out the string "500" for the body of the response, and calls the
function `finishWith` on that response.

Remember `Alternative` semantics for the `Snap` monad? `finishWith` is a
function you can call to short-circuit the action so that no further `Snap`
actions will be run.

Let's test this with some childish language to see that it works.

	$ curl -i localhost:8000/echo/poop
	HTTP/1.1 500 Internal Server Error
	Date: Mon, 10 May 2010 07:08:32 GMT
	Server: Snap/0.pre-1
	Transfer-Encoding: chunked

	500$

## What Now?

We hope we've whetted your appetite for using Snap. From here on out you should
take a look at the [API documentation](/docs/latest/snap-core/index.html),
which explains many of the concepts and functions here in further detail.

You can also come hang out in `#snap-framework` on `freenode`.
