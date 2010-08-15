| title: Announcing: Snap Framework v0.2.9
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-08-15T14:30:00-0400
| updated: 2010-08-15T14:30:00-0400
| summary: Release notes for version 0.2.9 of the Snap Framework.

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.9. Most of the
changes since 0.2.8 are bugfixes and testing improvements.

Changes since 0.2.8.1
=====================

### Bugfixes:

 - Fixed an HTTP POST body parsing bug where we would not accept the valid MIME
   type "`application/x-www-form-urlencoded; charset=UTF-8`".


### Improvements:

 - [Test coverage for
   `snap-core`](http://buildbot.snapframework.com/job/snap-core/HPC_Test_Coverage_Report/) is now close to 100% in most modules.

 - Some small performance improvements in sending chunked `Transfer-Encoding`s.

 - Miscellaneous code cleanup.


### Dependency changes:

 - We now require [cereal](http://hackage.haskell.org/package/cereal) 0.3.

 - In libev mode, we now require
   [hlibev](http://hackage.haskell.org/package/hlibev) 0.2.8.

 - Relaxed [criterion](http://hackage.haskell.org/package/criterion) bounds in
   the `snap-server` testsuite.
