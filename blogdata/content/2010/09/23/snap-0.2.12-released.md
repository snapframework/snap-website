| title: Announcing: Snap Framework v0.2.12
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-09-23T17:56:10-0400
| updated: 2010-09-23T17:56:10-0400
| summary: Release notes for version 0.2.12 of the Snap Framework.

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.12.

Changes since 0.2.11
=====================

### Improvements:

 - The Snap debugging system has been reworked; before, to get debugging output
   from a Snap program, you needed to build `snap-core` with the `-fdebug` flag
   and rebuild your entire webapp stack. Now, in a stock Snap installation, you
   can set the environment variable `DEBUG=1` to get Snap to produce debugging
   output to stderr.

   There is a slight performance hit associated with this change (in that GHC
   can no longer inline calls to `debug` because we use `unsafePerformIO` to
   read the `DEBUG` environment variable to pick a debug implementation at
   runtime) --- if you simply cannot stand the overhead of a no-op function
   call, pass the `no-debug` flag when installing `snap-core`:

~~~~~~~~ {.shell}
$ cabal install snap-core -fno-debug
~~~~~~~~

 - fixed a performance issue related to buffer handling in responses using
   `Transfer-Encoding: chunked`.

 - Many more tests have been written for `snap-server`; 
   [test coverage](
     http://buildbot.snapframework.com/job/snap-server/HPC_Test_Coverage_Report/)
   for expressions is up in the 90%+ range now for most modules.


### API Changes:

 - The function `unsafeDetachRequestBody` never worked and wasn't designed
   properly; it's been replaced with a much improved `transformRequestBody`
   function which a) actually works, and b) doesn't have issues re:
   unsafeness. Normally, following the package version policy would necessitate
   a minor version bump for this (to 0.3) but since the original function never
   worked in the first place, nobody could possibly be using it!


### Bugfixes:

 - The 'redirect' function would not allow you to set headers in the redirect
   response; this has been fixed.

 - GZip filter: set `Vary: Accept-Encoding` on gzipped responses (for caching
   proxies)

 - The type of `finishWith` has been changed from:

 - Shutdown logic for both `libev` and standard backends has been reworked and
   improved; no more 2 second delays!

<div>

~~~~~~~~~~~ {.haskell}
finishWith :: Response -> Snap ()
~~~~~~~~~~~ {.haskell}

to:

~~~~~~~~~~~ {.haskell}
finishWith :: Response -> Snap a
~~~~~~~~~~~ {.haskell}
</div>



