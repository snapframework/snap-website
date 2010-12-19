## Installation

The exposed Snap API is on the same level of abstraction as Java Servlets. If
you understand servlets, most of the rest of the tutorial should be very
self-explanatory. Even if you don't, have no fear!  The following tutorial only
expects that you know a little bit of Haskell.

Before we dive into writing our first Snap web application, let's do a quick
overview of the parts of the Snap framework. Currently Snap is divided into
four components:


  - `snap-core` is the core of Snap. It includes type definitions and all code
  that is server-agnostic.

  - `snap-server` is an HTTP server library built on top of `snap-core`. It
  currently includes a backend using stock Haskell socket I/O and a backend
  which uses the [libev](http://software.schmorp.de/pkg/libev.html) O(1) event
  loop library.

  - `heist` is the X(HT)ML templating library. You do not need it to use Snap,
  but you are certainly welcome to.

  - `snap` is a library which contains some useful routines for building Snap
  sites and a `snap` executable which generates a skeleton project to get you
  started.
  
To install Snap, simply use `cabal`. It's up to you whether or not you want to
use the Heist templating library; that said, as of version 0.3 heist is a
dependency for the snap project starter (although, again, you're not forced to
use it), and our example below does use it.

~~~~~~ {.shell}
$ cabal install snap
~~~~~~

## Hello, Snap!

To generate a skeleton Snap web application, the `snap` package installs an
executable `snap` into `$HOME/.cabal/bin`. We can use that to create our "Hello
Snap" application.

(If you do not want to use `heist`, you should execute `snap init -b` instead
of `snap init` for a bare-bones skeleton that does not depend on `heist`.)

~~~~~~ {.shell}
$ mkdir hello-snap
$ cd hello-snap
$ snap init
~~~~~~

We now have a skeleton Snap project with a `.cabal` file and a source
directory. Install it, run it, and see what your browser thinks of it.

~~~~~~ {.shell}
$ cabal install
$ hello-snap -p 8000
$ curl 'http://localhost:8000/'; echo
<!DOCTYPE html PUBLIC "-//W3C//DTD XHT
...
~~~~~~

Make sure to try the some of the echo examples. When you are satisfied, we can
kill the server by hitting `CTRL-C`.


### Under the Hood

Peeking in the `src` directory, we find the haskell files responsible for the
simple demo page we start with.  

~~~~~ {.shell}
$ cd src
$ ls 
Application.hs  Main.hs  Site.hs
~~~~~

`Main.hs` contains the `main` entry point to the application. The default
skeleton project contains some C preprocessor statements for optionally
configuring the server to use [hint](http://hackage.haskell.org/package/hint)
to dynamically load the site in "development" mode. If we ignore this for now,
we see that `main` just starts up `snap-server` using our site's initialization
and web handling routines:

~~~~~~ {.haskell}
main :: IO ()
main = quickHttpServe applicationInitializer site
~~~~~~

The `Application.hs` file defines the `Application` type, which is the type of
our site's web handlers. `Application` extends the default `Snap` handler type
with some application-specific state, also defined in `Application.hs`.

A handler (informally, for now) is a function, run by the Snap server, which
takes an HTTP request and generates the server's Response, with a lot of the
complexity sort of nudged away under the rug. The handlers currently being used
by your sample app are defined in `Site.hs`.

For example, here's the top-level Handler used by your sample app:

~~~~~~ {.haskell}
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> fileServe "resources/static"
~~~~~~

This sets up a routing table for the site's URLs: requests for the "`/`" URL
get routed to the "`index`" handler, and requests for "`/echo/foo`" get routed
to the "`echo`" handler after we set "`stuff=foo`" in the request's parameter
mapping.

The second half of the site handler, after the main routing table, handles
serving any files from the disk. The `a <|> b` syntax means: "try `a`, and if
it fails, try `b`". In this case, if the user requests an URL that doesn't
match any of the entries in the routing table --- like "`/screen.css`", for
example --- the `fileServe` function will attempt to find the file under the
"`resources/static`" directory, and will serve it back to the user if it is
found. If the file is *not* found, the "fileServe" handler will fail, causing
the `site` handler to fail, which will cause Snap to produce a "404 Not Found"
response.


Next, let's take a look at the `echo` handler:

~~~~~ {.haskell}
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" message) $ render "echo"
  where
    decodedParam p = fromMaybe "" <$> getParam p
~~~~~

The `echo` handler takes the `stuff` parameter out of the parameters mapping
(remember, "`stuff`" was bound using `route` above), binds its value to a
`heist` tag, and then renders the "echo" template.

A [lengthier Heist tutorial](/docs/tutorials/heist/) is available, but here's
what you need to know for now: `heist` serves XHTML templates, but with a
twist: a tag element can be rebound to a Haskell function which replaces it
with some programmatically-generated markup. In our case, the `echo` template
lives in `resources/templates/echo.tpl`, and the "`<message>`" tag has been
redefined to be substituted for the text that was entered by the user. The
template looks like this:

~~~~~ {.html}
<body>
  <div id="content">
    <h1>Is there an echo in here?</h1>
  </div>
  <p>You wanted me to say this?</p>
  <p>"<message/>"</p>
  <p><a href="/">Return</a></p>
</body>
~~~~~


## What Now?

We hope we've whetted your appetite for using Snap. From here on out you should
take a look at the [API documentation](/docs/latest/snap-core/index.html),
which explains many of the concepts and functions here in further detail.

You can also come hang out in
[`#snapframework`](http://webchat.freenode.net/?channels=snapframework&uio=d4)
on [`freenode`](http://freenode.net/).
