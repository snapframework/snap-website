| title: Announcing: Snap Framework v0.13
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2013-09-09T14:10:00-0400
| updated:   2013-09-09T14:10:00-0400
| summary: Release notes for Snap 0.13

The Snap team is happy to announce the release of version 0.13 of the Snap
Framework.  We would like to thank Edward Kmett who helped with some of the
broad design ideas incorporated in this release.

The Heist API changed significantly in this release.  If you are migrating an
existing app to 0.13, check out the following diffs to see how the migration
was done in some real packages.

* [digestive-functors-heist](https://github.com/jaspervdj/digestive-functors/compare/cc2ae4e3ff94b335444d27fdc0533c102ba98e1a...5a9d3fb75be6db58145f00b06c763a06f7b50f6c)
* [snap-extras](https://github.com/ozataman/snap-extras/compare/e44057dbe3d622883d1ea44bb3a97d5465e6049a...557ea1882a485f3509200db7d5733fef6f9cfc70)

## Major Changes

  - The API for compiled Heist has now stabilized.  The new API is much
    simpler than the old one.  The old API had inconsistent use of `n a`,
    `RuntimeSplice n a`, and `Promise a`, which made it difficult to
    understand how things should be composed.  Since those three things are
    different incarnations of roughly the same general idea, we were able to
    simplify the high-level API to just use `RuntimeSplice n a` everywhere.

  - The low level Heist API was moved to its own module.  The high-level API
    referred to above is much simpler, but slightly restricted in its power.
    If you need to write compiled splices with special control flow, then you
    will need to import the low level API and work with promises directly.

  - Heist now has new syntax for writing splices.  The old alist syntax made
    behavior unclear when the same splice name occurred more than once.  Also,
    the actual syntax for alists is not incredibly clean either.  The new
    syntax is defined in the Heist.SpliceAPI module and leverages do notation
    and a few infix operators to create elegant syntax for defining splices
    while at the same time making semantics more clear.

  - The snaplet testing API has been modified to allow you to choose an
    environment.

## Bugfixes / minor improvements

  - Fixed doctype handling in compiled templates.

  - We added a new function getEnvironment to Snap.Snaplet that allows
    snaplets to get the current environment.

