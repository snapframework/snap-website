| title: Announcing: Snap Framework v0.1
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-05-22T01:25:00-0400
| updated: 2010-05-22T01:25:00-0400
| summary: The first public release of the Snap Framework is now available. Snap is a simple and fast web development framework for unix systems, written in the Haskell programming language.

To coincide with [Hac Phi 2010](http://www.haskell.org/haskellwiki/Hac_%CF%86),
the Snap team is happy to announce the first public release of the Snap
Framework, a simple and fast Haskell web programming server and library for
unix systems. For installation instructions, documentation, and more
information, see our website at [snapframework.com](http://snapframework.com/).

Snap is well-documented and has a test suite with a high level of code
coverage, but it is early-stage software with still-evolving interfaces. Snap
is therefore most likely to be of interest to early adopters and potential
contributors.

Snap is BSD-licensed and currently only runs on Unix platforms; it has been
developed and tested on Linux and Mac OSX Snow Leopard.

Snap Features:

 * A simple and clean monad for web programming, similar to happstack's but
   simpler.

 * A *fast* HTTP server library with an optional high-concurrency backend
   (using libev).

 * An XML-based templating system for generating xhtml that allows you to bind
   Haskell functionality to XML tags in your templates.

 * Some useful utilities for web handlers, including gzip compression and
   fileServe.

 * Iteratee-based I/O, allowing composable streaming in O(1) space without any
   of the unpredictable consequences of lazy I/O.

If you have questions or comments, please contact us on our [mailing
list](http://mailman-mail5.webfaction.com/listinfo/snap) or in the
[#snapframework](http://webchat.freenode.net/?channels=snapframework&uio=d4)
channel on the freenode IRC network.
