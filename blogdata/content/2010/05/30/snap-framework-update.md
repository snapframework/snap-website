| title: Snap Framework: What's new this week?
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-05-30T02:42:00-0400
| updated: 2010-05-30T14:00:00-0400
| summary: Summarizes the changes between snap-core/-server v0.1.1 (released last week) and snap-core v0.2.5 (released today).

Hi all,

Since we put out [the Snap framework](http://snapframework.com/) last weekend,
we've been working like busy beavers on squashing correctness and performance
bugs.  Updated haddocks/etc should be up on our website by tomorrow
afternoon. Here's a short list of the changes in Snap this week:

 - **WINDOWS SUPPORT** thanks to Jacob Stanley (a.k.a. "jystic").

 - A fix for a grave performance bug with `Transfer-encoding: chunked`; we
   weren't buffering its input, causing lots of tiny http transfer chunks for
   certain pathological input, ruining performance. (This is the one [Michael
   Snoyman reported](http://www.snoyman.com/blog/entry/bigtable-benchmarks/)
   btw.) Switching to buffering its input increased performance on this test by
   at least an order of magnitude.

 - Huge improvements to the `libev` backend for `snap-server`, including fixing
   a correctness/hang bug and an edge-/level-triggering issue. Performance
   should be improved to the point where the `libev` backend should be
   considered the "go-to" setup for production `snap-server` deployments.

 - Improved timeout handling in the "simple"/stock haskell `snap-server`
   backend. This costs us some performance on the stock backend, but
   correctness is more important (and users wanting maximum performance should
   stick with the `libev` backend).

 - Fixed an `attoparsec-iteratee` bug that resulted in spurious "parser did not
   produce a value" messages cluttering `error.log`.

 - Fixed a localtime/GMT timezone bug which prevented static files from being
   recognized as "not modified."

 - Fixed an HTTP cookie reading bug in `snap-server`.

 - Killed several space leaks.

 - Fixes to the way Snap handles `accept-encoding` headers in the GZip code --
   requests from Konqueror and Links are no longer incorrectly rejected.

 - The `snap` command-line tool now has an option to not depend on heist.

 - Exposed error logging to the `Snap` monad.

 - ..and a whole host of smaller additions/improvements....

