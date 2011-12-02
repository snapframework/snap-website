| title: Announcing: Snap Framework v0.7
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2011-12-01T20:35:00-0500
| updated: 2011-12-01T20:35:00-0500
| summary: Release notes for Snap 0.7

The Snap team would like to announce the release of version 0.7 of the Snap
Framework.  There are some major changes in this release that are not
backwards compatible, so be sure to read these release notes before upgrading.

## New features / improvements

  - Heist: We realized that the `$()` syntax Heist uses for accessing splices
    inside of HTML attributes conflicts with the jQuery '$' function. We
    changed the syntax to ${} to fix this. This is a major breaking change, so
    we added a temporary compatibility mode to Heist that you can enable by
    using the `useOldAttributeSyntax` function to modify your TemplateState. We
    will remove this compatibility mode in the next major release, so update
    your templates ASAP!

  - Snaplets: added `getRoutePattern` (and `setRoutePattern`) which allows you
    to find out the actual route string that matched the currently running
    handler.

  - Heist Snaplet: added `heistInit'` to make it possible to use Heist's onLoad
    hooks.

  - Added the convenience functions `getsRequest` and `getsResponse` to
    `Snap.Core`.

  - Fixed [a bug](https://github.com/snapframework/snap-core/issues/107) in how
    the `route` function handled url-encoded path segments.

  - `snap-server`: added primitives escape the http session in order to support
    websockets.


## API Changes

  - `heist`: changed the `callTemplate` function to take general splices
    rather than constant text splices, and simplified the return type.  The
    old functionality now exists with a simpler return type as
    `callTemplateWithText`.

  - `heist`: Since we are making major breaking changes, we decided to change
    the name of TemplateState to the more appropriate HeistState.  The old
    name still exists as a type alias, but is marked as deprecated.  Update
    your code now because we will remove all the deprecated functionality in
    the next major release.


## Dependency changes

  - Increased the upper version bound on the time package from 1.4 to 1.5.

  - Increased the upper version bound on the regex-posix package from 0.94.4
    to 0.95.2.

  - Increased the upper version bound on the case-insensitive package from 0.4
    to 0.5.

  - Increased the lower version bound on attoparsec to 0.10.

  - Increased the version constraints on attoparsec-enumerator to 0.3.*.

