| title: Announcing: Snap Framework v0.2.15
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-10-27T02:07:00+0200
| updated: 2010-10-27T02:07:00+0200
| summary: Release notes for version 0.2.15 of the Snap Framework.

The Snap team is pleased to announce the release of Snap 0.2.15.

Changes since 0.2.14
====================

### Improvements:

  - Improved the way that the stock `snap-server` backend handles timeouts;
    before we were using [Johan Tibell's
    trick](http://blog.johantibell.com/2010/04/generational-garbage-collection-and.html)
    of keeping a list of edits to the master threads table behind an `IORef`,
    and then applying these edits in a worker thread which runs periodically to
    kill timed-out connections.

    During some benchmark testing we've been doing for an upcoming article, we
    noticed that this approach was still causing too much blackhole contention
    on the `IORef` --- worker threads were spending too much time fighting over
    who got to modify the list of edits. In `snap-server` 0.2.15, we've
    switched to a striped locking scheme, in which we manage a vector of
    priority search queues, each guarded by a lock. We pick which queue to
    register with based on the hash of the worker thread's `ThreadId`.

    In our testing this change has improved throughput by 50% in absolute
    terms. You can browse the source code for [the new
    module](http://github.com/snapframework/snap-server/blob/master/src/Snap/Internal/Http/Server/TimeoutTable.hs)
    on our github page.


### Bugfixes

  - The `snap` project starter executable wasn't building properly due to some
    transitive dependency conflicts over our recent upgrade to
    [`text`](http://hackage.haskell.org/package/text) version 0.10. A new
    version of `heist` has been pushed to upgrade our `hexpat`, and the project
    starter has been modified to match: please let us know if you encounter
    problems.
