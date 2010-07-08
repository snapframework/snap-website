| title: Announcing: Snap Framework v0.2.8
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-07-08T19:11:47-0400
| updated: 2010-07-08T19:11:47-0400
| summary: Release notes for version 0.2.8 of the Snap Framework.

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.8.

Changes since 0.2.7
===================

### Improvements:

 - Changed the way that `snap-server` handles its thread table in the `libev`
   backend. The old code was causing blackhole/lock contention on the shared
   table; we switched to a new [concurrent hash
   map](http://github.com/snapframework/snap-server/blob/master/src/Data/HashMap/Concurrent.hs)
   container type, which improved performance on the "PONG" benchmark from ~16k
   req/s to about ~22k req/s on mightybyte's machine (a >35% improvement!).

### Changes in behaviour:

 - When POST bodies come in with `Content-Type:
   application/x-www-form-urlencoded`, Snap auto-parses them for you, entering
   the POST parameters into its parameter map. This was preventing a
   [wai](http://hackage.haskell.org/package/wai) backend for Snap, because
   `wai` expects to be able to read the POST body itself. We've changed
   `snap-server` so that the POST body is still automagically processed, but
   you can still re-read the raw bytes if you want to. This means parsing POST
   bodies twice for `wai` handlers, but it at least makes writing a `wai`
   adapter for Snap possible.
