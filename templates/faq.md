<div id="faqspage">
## Frequently Asked Questions

<div class="faqs">

1. [How can I report a bug in Snap?         ](#how-can-i-report-a-bug-in-snap)
1. [Is anyone using Snap in production?     ](#is-anyone-using-snap-in-production)
1. [Where's the high-level functionality?   ](#wheres-the-high-level-functionality)
1. [Why can't I install Snap?               ](#why-cant-i-install-snap)
1. [I am getting a "cannot find normal object file" error when trying to build Snap. Why?](#i-am-getting-a-cannot-find-normal-object-file-error-when-trying-to-build-snap.-why)
1. [How do I install Heist on Windows?      ](#how-do-i-install-heist-on-windows)
1. [How do I get the libev backend working? ](#how-do-i-get-the-libev-backend-working)
1. [How can I get debugging output?         ](#how-can-i-get-debugging-output)
1. [When I run snap with multiple cores, throughput plummets. What's going on?](#when-i-run-snap-with-multiple-cores-throughput-plummets.-whats-going-on)
1. [How can I help?                         ](#how-can-i-help)

</div>



### How can I report a bug in Snap?

Found a bug in Snap? Please create a ticket on our
[issue tracker.](http://github.com/snapframework/snap-core/issues)


### Is anyone using Snap in production?

Yes!  Here is a list of sites that we know of that use Snap.  Let us know if
you know of others:

  -  [Darcsden](http://darcsden.com) is a nifty github-like source
     code hosting site for darcs.

  -  [http://snapframework.com](http://snapframework.com) (this site)

  -  [http://ego.fm           ](http://ego.fm) (not always up)


### Where's the high-level functionality?

Our goal is for Snap to be a very fast, stable, *high-level* web framework at
or above the same level of abstraction as frameworks like Ruby on Rails,
Django, etc.  During early planning and development we concluded that to
accomplish this goal we needed to build our own web server and API to interface
with it.  This was an unanticipated detour, and we will resume working on
higher-level functionality when the core has stabilized.


### Why can't I install Snap?

First, make sure you have `"$HOME/.cabal/bin"` at the beginning of your path.

You may have an old version of Cabal.  Try running "`cabal update && cabal
install Cabal`".  After this, "`cabal --version`" should say that you're using
version 1.8.0.4 of the Cabal library or higher.  After you do this, try
installing Snap again.

If that doesn't work, and you're getting an error that mentions `monads-fd` or
`transformers`, try running "`cabal install --reinstall monads-fd`"
(or `transformers`).

If you're still having trouble, please email our [mailing
list](http://mailman-mail5.webfaction.com/listinfo/snap) or contact us on our
[IRC channel](http://webchat.freenode.net/?channels=snapframework&uio=d4)
(`#snapframework` on [freenode](http://freenode.net/)).


### I am getting a "cannot find normal object file" error when trying to build Snap. Why?

If you try to build Snap in profiling mode, sometimes you will encounter this error:

~~~~~~~~ {.shell}
src/Snap/Starter.hs:1:0:
     cannot find normal object file `dist/build/snap/snap-tmp/Snap/StarterTH.o'
~~~~~~~~

GHC has a bug in which it gets confused about where to look for object files
when a) using template haskell, and b) compiling in profiling mode. The
workaround is to compile the library without profiling, then reconfigure and
rebuild:

~~~~~~~ {.shell}
$ cabal configure
$ cabal build
$ cabal configure -p
$ cabal build
~~~~~~~

### How do I install Heist on Windows?

Heist needs a C XML library called expat.  See [this article by Jacob
Stanley](http://jystic.com/2010/10/07/installing-heist-and-hexpat-on-windows/)
for more information.

### How do I get the libev backend working?

To install Snap's [libev](http://software.schmorp.de/pkg/libev.html) backend,
ensure you have the `libev` development libraries installed on your system and
pass the `-flibev` flag to `cabal` when you install `snap-server`:

~~~~~~ {.shell}
$ cabal install snap-server -flibev
~~~~~~

If you get an undefined symbol "`EVFLAG_SIGNALFD`" then you'll need to
install the latest [libev](http://software.schmorp.de/pkg/libev.html)
from [source](http://dist.schmorp.de/libev/).


### When I run snap with multiple cores, throughput plummets. What's going on?

The parallel GC introduced in GHC 6.10 doesn't seem to play very well with
Snap. If you turn parallel GC off (using the "`-qg`" flag, e.g. "`./foo-website
+RTS -N -qg`") throughput should improve dramatically.


### How can I get debugging output?

With recent (>0.2.12) snap-core, if you set the environment variable `DEBUG=1`
then Snap will produce debugging output to stderr. There is a *very* slight
performance penalty associated with this feature; if you are in a production
setting and require speed at all costs, you can disable debug output when
building `snap-core` by passing the `no-debug` flag to `cabal install`:

~~~~~~~~ {.shell}
$ cabal install snap-core -fno-debug
~~~~~~~~


### How can I help?

##### Use Snap to build real websites.

This is perhaps the best way to help.  Let us know what issues you encounter
and work on fixing the ones you care about most.  If you are unable to fix a
problem, you can still help by writing an automated test case that detects the
problem.

It's also very likely that infrastructure you create in the course of building
a real website can be generalized and merged into Snap.  Much of Snap's
[higher-level functionality](#wheres-the-high-level-functionality) should
become more evident as we find code patterns common to real-world Snap
websites.  Communicating real-world Snap development experiences to us is a
great way to contribute to this effort.

##### Develop automated memory leak and performance regression testing.

Currently our top priority is working out correctness and performance issues in
the server.  We have a CI build server that automatically runs all our test
cases, but we don't have an automated system to test for performance and memory
leaks.  This would be a very helpful addition.


##### Improve test cases and code coverage.

While not an exotic task, expanding our test suite can contribute significantly
to the stability of the project.


##### Improve documentation and tutorials.

It's easy for documentation to get out of date.  We try to keep it up-to-date,
but we can always use more eyes to catch things that slip through the cracks.


</div>
