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
use the Heist templating library; that said, as of 0.3 heist is a dependency 
for snap, and hello-snap below does use heist. 

~~~~~~ {.shell}
$ cabal install snap
~~~~~~

## Hello Snap

To generate a skeleton Snap web application, the `snap` package
installs an executable `snap` into `$HOME/.cabal/bin`. We can use that to
create our "Hello Snap" application.

(If you did not install `heist`, you should execute `snap init -b` instead of
`snap init` for a bare-bones skeleton that does not depend on `heist`.)

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

Make sure to try the some of the echo examples. 
When you are satisfied, we can kill the server.  

~~~~~~ {.shell}
$ fg
hello-snap -p 8000
^C
~~~~~~

<!-- AlexeyMK's thoughts: At this point, it's certainly possible to take
     the tutorial on a tour of the Snap Monad, the idea of an Application as
     a snap extension. Alternately, it's possible to do a walkthrough of the
     actual source that generates the hello world example, without diving too
     deeply into 'why things work' until we've let the developer actually do
     something.  Both approaches have merit; given my current knowledge, I'm 
     going to go ahead and write a high-level intro to how stuff works below.
     This may not be the best place for such an intro and I'd love to get 
     feedback on that point.
-->

### Under the Hood

Peeking in the `src` directory, we find the haskell files responsible for the
simple demo page we start with.  

~~~~~ {.shell}
$ cd src
$ ls 
Application.hs  Main.hs  Site.hs
~~~~~

`Main.hs` contains the entry point to the application. In its simplest form, 
main does nothing more than ask the `site` handler to figure out what to do 

~~~~~~ {.haskell}
main :: IO ()
main = quickHttpServe applicationInitializer site
~~~~~~

`Application.hs` defines the Application type, used to define handlers across
the site.  A handler (informally, for now) is a way to get from an HTTP Request
to an HTTP Response, with a lot of the complexity sort of nudged away under 
the rug. The handlers currently being used by your sample app are in `Site.hs`.

For example, here's the top-level Handler used by your sample app:

~~~~~~ {.haskell}
site :: Application ()
site = route [ ("/",            index)
             , ("/echo/:stuff", echo)
             ]
       <|> fileServe "resources/static"
~~~~~~

This reads reasonbly well, especially if you've worked with django or any other
web framework with a `urls` equivalent.  Effectively, route is 
saying, "does the request look anything like this? If so, great, pass the
responsibility for creating a response to this function." In the echo case, 
`stuff` is used as a named argument for whatever comes after `/echo/`, used by 
the `echo` handler below.  

Also note the `<|> fileServe` part.  `<|>` comes from the `Applicative` type,
and is here used to say "try the first thing, and if that doesn't work, try
this second thing." `fileServe` is a handler that lets you serve static
content, such as CSS or images, as visible in some directory.

Next, let's take a look at the `echo` handler:

~~~~~ {.haskell}
echo :: Application ()
echo = do
    message <- decodedParam "stuff"
    heistLocal (bindString "message" message) $ render "echo"
  where
    decodedParam p = fromMaybe <*> urlDecode <$> fromMaybe "" <$> getParam p
~~~~~

First, says `echo`, let's pull that named `stuff` argument out, make sure it is
valid and healthy, and store it as `message`.  Then we use `heist's render` 
function to do the rest of the work and serve the `echo` template, but not 
before sticking `message` into the state of the the monad being passed around.

## A note on Heist

A [lengthier Heist tutorial](/docs/tutorials/heist/) is available, but here's
what you need to know for now: `heist` serves templates which you can 
parametrize as needed. In our case, the `echo` template lives in 
`resources/templates/echo.tpl`. The template looks something like this:

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

This is mostly just HTML; no big deal. There is, however, a `<message/>` tag
hanging out, not particularly interesting or fun by itself, but pretty useful
when we ask heist to render `echo` with `message` bound to whatever argument 
was passed to us as `stuff`, in the URL path after `/echo/...`.  And thus the 
journey is complete, and an argument has travelled from being part of the path
to the `echo` handler, bound in the monadic state of the application, and passed
into a template to be rendered upon the page. 

<!-- Edited by AlexeyMK until here -->

## What Now?

We hope we've whetted your appetite for using Snap. From here on out you should
take a look at the [API documentation](/docs/latest/snap-core/index.html),
which explains many of the concepts and functions here in further detail.

<!-- AMK Note: this should in no way be the last tutorial. 
     Tutorials are badly needed on 
     - the intermezzo part and how things actually work
     - where in the docs to look
     - snap extensions-->

You can also come hang out in
[`#snapframework`](http://webchat.freenode.net/?channels=snapframework&uio=d4)
on [`freenode`](http://freenode.net/).
