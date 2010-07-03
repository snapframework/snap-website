| title: Announce: Snap Framework 0.2.7
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-07-03T01:52:31-0400
| updated: 2010-07-03T01:52:31-0400
| summary: Release notes for version 0.2.7 of the Snap Framework.

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.7. This release
contains **critical fixes for correctness bugs** and all Snap users should
upgrade to the latest version (including `snap-server` 0.2.7.1, a point fix for
an embarassing release issue) at their earliest convenience.

Changes since 0.2.6
===================

### New features:

 - the "snap" project starter was reorganized by new Snap contributor Shane
   O'Brien (duairc).

 - The HTTP parser in `snap-server` has a couple of new criterion benchmarks
   courtesy of Aycan Irican.

 - the default/simple Snap backend should now have properly functioning timeout
   support.


### Fixes:

 - fixed a serious HTTP spec bug; the HTTP spec says that 304 responses are
   never to contain message bodies, but we were sending ours with
   "`Transfer-encoding: chunked`", causing a spurious "0\\r\\n\\r\\n" sequence to
   cause pipelined HTTP sessions to go out of sync.

 - fixed a bug in the `route` function related to variable capture.

 - fixed a couple of minor bugs related to iteratee buffering.

 - squashed a space leak.

 - some fixes related to `sendfile()` support, especially relating to
   timeouts. On a temporary basis this has required us to pull in quite a bit
   of internal code from the `sendfile` library, because that library uses
   GHC's IO manager for event handling. (Not so good for us on libev!)


Upcoming in 0.3
===============

Carl and Shane have been working very hard on the upcoming 0.3 branch
release. The `snap` project starter executable has been spawned out of the
`snap-core` repository and completely rewritten; the new version features a
"development mode" which runs your web handlers using
[hint](http://hackage.haskell.org/package/hint). This will allow changes in
your Haskell code to be reflected *instantly* in the server output, without the
overhead of a shutdown/startup cycle on the server. We're really excited about
this feature, it gets us a lot closer to having the best of both worlds: rapid
development to rival PHP or Python, as well as high performance in "production"
mode.
