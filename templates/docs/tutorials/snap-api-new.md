## Examining Hello World

In the [Quick Start Guide](/docs/quickstart), installed Snap and created a
template "hello world" application.  Here we'll look at what's inside the
application and describe basic use of Snap's web server API.

`snap init` creates two files in the `src` directory, General.hs and Main.hs.
General.hs contains general-purpose code that will eventually be integrated
into the Snap framework so you won't have to carry this code with your
application.  For now, we've left it out to avoid unnecessary project
dependencies.  You don't need to worry about General.hs unless you want an
in-depth understanding of the infrastructure.  Here's the important code in
Main.hs:

~~~~~~ {.haskell}
site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    dir "seeform" (method POST showParams) <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (fileServe ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
~~~~~~

The behavior of this code can be summarized with the following rules:

1. If the user requested the site's root page (http://mysite.com/), then
return a page containing the string "hello world".
2. If the user requested /foo, then return "bar".
3. If the user requested /echo/xyz, then return "xyz".
4. If the request URL begins with /static/, then look for files on disk
matching the rest of the path and serve them.
5. If none of these match, then Snap returns a 404 page.

Let's go over each of the Snap API functions used here.

##### [`ifTop`](/docs/latest/snap-core/Snap-Types.html#v%3AifTop)

`ifTop` only executes its argument if the client requested the root URL.  Use
this for your home page.  It can also be combined with the `dir` function to
define your index handler for a certain URL directory.

##### [`writeBS`](/docs/latest/snap-core/Snap-Types.html#v%3AwriteBS)

`writeBS` appends a strict ByteString to the response being constructed.  Snap
also provides an analogous function `writeLBS` for lazy ByteStrings.  You can
also use the functions `writeText` and `writeLazyText` if you use Data.Text
instead of ByteString.

##### [`dir`](/docs/latest/snap-core/Snap-Types.html#v%3Adir)

`dir` runs its action only if the request path starts with the specified
directory.  You can combine successive dir calls to match more than one
subdirectory into the path.










If you're not familiar with Haskell, you may be wondering about the `<|>`.  It
is simply a binary operator that evaluates its first argument, and if it
failed, evaluates the second.  If the first argument succeeded, then it stops
without evaluating the second argument.

The `site` function uses `<|>` to connect three different functions that guard
what page gets rendered.  First, the `ifTop` function runs.  This function
succeeds if the requested URL is `http://site.com/`.  If that happens, then
Snap sends a response of "hello world".  Otherwise the `route` function is
executed.

`route` takes a list of (route, handler) tuples and succeeds returning the
result of the associated handler of the route that matches.  If no route
matches, then execution is passed on to `fileServe`.  



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
consult the [API
documentation](/docs/latest/snap-core/Snap-Types.html#10).

We hope this quick and dirty introduction to iteratee I/O has shed some light
on using Snap. Some further discussion, the original talk and other iteratee
resources may be found on [Oleg's site](http://okmij.org/ftp/Streams.html). You
can also consult the [iteratee package API
docs](http://hackage.haskell.org/package/iteratee), and the [Haskell
Wiki page on iteratees](http://www.haskell.org/haskellwiki/Iteratee).

### Routing

Our goal is to get our toy application to spit back a portion of the request
URL. That is, I want the following behavior:

~~~~~~ {.shell}
$ curl localhost:8000/echo/ohsnap; echo
ohsnap
~~~~~~

So first, we need to tackle how to route URLs. Snap provides two types of
routing functions: combinators and
[`route`](/docs/latest/snap-core/Snap-Types.html#v%3Aroute). Some combinators
are as follows:

- [`<|>`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Applicative.html#v%3A%3C%7C%3E) `:: Snap a -> Snap a -> Snap a`
- [`ifTop`](/docs/latest/snap-core/Snap-Types.html#v%3AifTop) `:: Snap a -> Snap a`
- [`dir`](/docs/latest/snap-core/Snap-Types.html#v%3Adir) `:: ByteString -> Snap a -> Snap a`
- [`method`](/docs/latest/snap-core/Snap-Types.html#v%3Amethod) `:: Method -> Snap a -> Snap a`

These do exactly what you might think they do. Calling `ifTop`:

~~~~~~ {.haskell}
foo :: Snap ()
foo = ifTop $ do ...
~~~~~~

takes an action in the Snap monad and transforms it to only run if the request
path is at the "top level" (specifically, when
[`rqPathInfo`](/docs/latest/snap-core/Snap-Types.html#v%3ArqPathInfo) is
empty). The `dir` combinator transforms the action to run if the request path
starts with the specified directory. The `method` combinator matches on
specific HTTP methods.

The astute reader will no doubt now see that these functions can be used in
conjunction with `Alternative` semantics of the `Snap` monad to do routing.
However, this kind of routing is expensive in that it can be O(n) in the number
of handlers. To remedy this, Snap also provides you with a function that routes
requests based on the request path in `O(log n)` time:

- `route :: [(ByteString, Snap a)] -> Snap a`

This function takes an associative list of paths and actions and combines it
all together into one action that does the routing. It also supports capturing
parts of the path into parameters.

Let's see this in action to route the path `echo/` to some echo action. Open
up `Main.hs` in your favorite editor and edit `site` to look like the
following.

~~~~~~~~~~~~~~~ {.haskell}
site :: Snap ()
site = route [ (""        , ifTop (writeBS "hello world"))
             , ("echo/:s" , echoHandler) ]
          <|> fileServe "."
~~~~~~~~~~~~~~~

The above code routes the top level empty path to the original "hello world"
action, and the path `echo/` to `echoHandler` (which we haven't written yet).
More importantly, when it routes `echo/`, it captures the next immediate path
component (not the rest of the path) into a parameter named `s`. We can access
`s`, or any other request parameter, with the function
[`getParam`](/docs/latest/snap-core/Snap-Types.html#v%3AgetParam). If the
request was neither to the top level nor to `echo/`, we try to find a file in
the current directory with the same name as the request path and serve that.

Moving on, let's write `echoHandler`, which should just spit back `s`.

~~~~~~~~~~~~~~~ {.haskell}
echoHandler :: Snap ()
echoHandler = do
    s <- getParam "s"
    writeBS $ maybe "" id s
~~~~~~~~~~~~~~~

Let's build our toy application and run it again. Now we have a perfectly
useless server that echos paths that start with `echo/`.

~~~~~~ {.shell}
$ curl localhost:8000/echo/foo; echo
foo
~~~~~~


### Error Handling

Pretend that we wrote this echo server for an elementary school, and the PTA
has requested that profanities be censored and unechoable. The parents of PTA
also happen to be not very bright and have taken the euphemism "4-letter word"
too literally. They have requested that we block all 4-letter words. For
arbitrary reasons, we've decided to respond to echo requests of 4-letter words
by sending a `403 Forbidden` error.

We'll need `ByteString` utility functions for this, so be sure to add the
following line somewhere in your list of `import`s.

~~~~~~~~~~~~~~~ {.haskell}
import qualified Data.ByteString as B
~~~~~~~~~~~~~~~

Let's rewrite our `echoHandler` as follows.

~~~~~~~~~~~~~~~ {.haskell}
echoHandler :: Snap ()
echoHandler = do
    s <- getParam "s"
    case s of
        Just s' -> if (B.length s' == 4) then badWord else (writeBS s')
        _       -> writeBS ""
  where
    badWord = do
        putResponse $
          setResponseStatus 403 "Forbidden" emptyResponse
        writeBS "Forbidden: four-letter word"
        r <- getResponse
        finishWith r
~~~~~~~~~~~~~~~

The `badWord` action sets the response status code to be 403 using a
combination of the
[`putResponse`](/docs/latest/snap-core/Snap-Types.html#v%3AputResponse) and
[`setResponseStatus`](/docs/latest/snap-core/Snap-Types.html#v%3AsetResponseStatus)
functions, writes out the body of the response, and calls the function
[`finishWith`](/docs/latest/snap-core/Snap-Types.html#v%3AfinishWith) on that
response.

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


### Serving It Up

So we've written our handler, how do we actually serve it and run the server?
Luckily you don't need to write any code as the generated `Main.hs` has the
glue code already. The relevant portion is reproduced below.

~~~~~~~~~~~~~~~ {.haskell}
    httpServe "*" port "myserver"
        (Just "access.log")
        (Just "error.log")
        site
~~~~~~~~~~~~~~~

There is no external server to plug into and no complicated directory
structure to manage. Everything is done programmatically. To embed the Snap
server inside your program, all you need to do is to pass
[`httpServe`](/docs/latest/snap-server/Snap-Http-Server.html#v%3AhttpServe)
some configuration parameters and your handler. Note that access and error
logging are optional, should you wish to forego them for performance reasons.


## What Now?

We hope we've whetted your appetite for using Snap. From here on out you should
take a look at the [API documentation](/docs/latest/snap-core/index.html),
which explains many of the concepts and functions here in further detail.

You can also come hang out in
[`#snapframework`](http://webchat.freenode.net/?channels=snapframework&uio=d4)
on [`freenode`](http://freenode.net/).
