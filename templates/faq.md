<div id="faqspage">
## Frequently Asked Questions

<div class="faqs">

1. [How can I report a bug in Snap?         ](#how-can-i-report-a-bug-in-snap)
1. [Is anyone using Snap in production?     ](#is-anyone-using-snap-in-production)
1. [Where's the high-level functionality?   ](#wheres-the-high-level-functionality)
1. [Why can't I install Snap?               ](#why-cant-i-install-snap)
1. [How do I get the libev backend working? ](#how-do-i-get-the-libev-backend-working)
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

  -  [http://ego.fm           ](http://ego.fm           )


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
(`#snapframework` on [freenode](http://freenode.net/).


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


### How can I help?

##### Use Snap to build real websites.

This is perhaps the best way to help.  Let us know what issues you encounter
and work on fixing the ones you care about most.  If you are unable to fix a
problem, you can still help by writing an automated test case that detects the
problem.


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
