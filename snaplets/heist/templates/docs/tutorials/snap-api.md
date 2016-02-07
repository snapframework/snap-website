## Examining Hello World

Before we dive into writing our first Snap web application, let's do a quick
overview of the parts of the Snap framework. Currently Snap is divided into
four components:

  - `snap-core` is the core of Snap.  It defines an API for interfacing with
  web servers and includes type definitions and all code that is
  server-agnostic.  This API is on the same level of abstraction as Java
  Servlets and is the focus of this tutorial.

  - `snap-server` is an HTTP server library that supports the interface
  defined in `snap-core`.

  - `heist` is the HTML templating library. You do not need it to use the
  above two libraries but you are certainly welcome to.

  - `snap` is a library that builds on the above three packages and provides
  higher-level abstractions for building complex websites.  It also contains
  a `snap` executable which can generate several different skeleton projects
  to get you started.

In the [Quick Start Guide](/docs/quickstart), we installed Snap and created a
template "hello world" application.  Here we'll look at what's inside the
application and describe basic use of Snap's web server API.

If you understand servlets and Haskell, most of the rest of the tutorial
should be very self-explanatory. Even if you don't, have no fear!  The
following tutorial only expects that you know a little bit of Haskell.

## Hello, Snap!

`snap init barebones` creates a single file in the `src` directory, Main.hs.
Here's the important code in Main.hs:

~~~~~~ {.haskell}
main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param
~~~~~~

If you haven not built the snap executable yet, you can see the full code for
this example
[here](https://github.com/snapframework/snap/blob/0.14-stable/project_template/barebones/src/Main.hs).

The behavior of this code can be summarized with the following rules:

1. If the user requested the site's root page (`http://mysite.com/`), then
return a page containing the string `"hello world"`.
2. If the user requested `/foo`, then return `"bar"`.
3. If the user requested `/echo/xyz`, then return `"xyz"`.
4. If the request URL begins with `/static/`, then look for files on disk
matching the rest of the path and serve them.
5. If none of these match, then Snap returns a 404 page.

Let's go over each of the Snap API functions used here.

### [`dir`](http://hackage.haskell.org/packages/archive/snap-core/0.9.0/doc/html/Snap-Core.html#v:dir)

`dir` runs its action only if the request path starts with the specified
directory.  You can combine successive dir calls to match more than one
subdirectory into the path.

### [`ifTop`](http://hackage.haskell.org/packages/archive/snap-core/0.9.0/doc/html/Snap-Core.html#v:ifTop)

`ifTop` only executes its argument if the client requested the root URL.  Use
this for your home page.  It can also be combined with the `dir` function to
define your index handler for a certain URL directory.

### [`writeBS`](http://hackage.haskell.org/packages/archive/snap-core/0.9.0/doc/html/Snap-Core.html#v:writeBS)

`writeBS` appends a strict ByteString to the response being constructed.  Snap
also provides an analogous function `writeLBS` for lazy ByteStrings.  You can
also use the functions `writeText` and `writeLazyText` if you use Data.Text
instead of ByteString.

If you're not familiar with Haskell, you may be wondering about the `<|>`.  It
is simply a binary operator that evaluates its first argument, and if it
failed, evaluates the second.  If the first argument succeeded, then it stops
without evaluating the second argument.

The `site` function uses `<|>` to connect three different functions that guard
what page gets rendered.  First, the `ifTop` function runs.  This function
succeeds if the requested URL is `http://site.com/`.  If that happens, then
Snap sends a response of "hello world".  Otherwise the `route` function is
executed.

You could build your whole site using `<|>` to connect different handlers
together, but this kind of routing is expensive because the amount of time it
takes to construct a request scales linearly with the number of handlers in
your site.  For large sites, this could be quite noticeable.  To remedy this,
Snap also provides you with the `route` function that routes requests based on the
request path in `O(log n)` time:

### [`route`](http://hackage.haskell.org/packages/archive/snap-core/0.9.0/doc/html/Snap-Core.html#v:route)

`route` takes a list of (route, handler) tuples and succeeds returning the
result of the associated handler of the route that matches.  If no route
matches, then `route` fails and execution is passed on to `serveDirectory`.

In a real application, you will want to use `route` for almost everything.
We didn't do it that way in this example because we wanted to demonstrate
more of the API.

### [`getParam`](http://hackage.haskell.org/packages/archive/snap-core/0.9.0/doc/html/Snap-Core.html#v:getParam)

`getParam` retrieves a GET or POST parameter from the request.  In this
example, the `route` function binds a captured portion of the URL to the
parameter `echoParam` so the associated handler can make easy use of it.
`echoHandler` checks to see whether a parameter was passed and returns the
value or an error message if it didn't exist.


## What Now?

We hope we've whetted your appetite for using Snap. From here on out you should
take a look at the [API
documentation](http://hackage.haskell.org/package/snap-core),
which explains many of the concepts and functions here in further detail.

You can also come hang out in
[`#snapframework`](http://webchat.freenode.net/?channels=snapframework&uio=d4)
on [`freenode`](http://freenode.net/) IRC.
