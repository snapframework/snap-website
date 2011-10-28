| title: Announcing: Snap Framework v0.6
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2011-10-28T00:25:00-0400
| updated: 2011-10-28T00:25:00-0400
| summary: Release notes for Snap 0.6

The Snap team is excited to announce the release of version 0.6 of the Snap
Framework.  This is a big release with a lot of new functionality.  Here is a
list of the major changes since 0.5.5:

## New features / improvements

  - The most significant new feature in 0.6 is the snaplet API.  It is a
    ground-up rewrite of our previous extensions system.  Snaplets are
    self-contained portions of websites that can be easily composed to create
    larger sites.  The snaplet infrastructure makes it simple to distribute
    snaplets for others to use, and provides a clear pathway for contributing
    to the community.  State management, built-in config file infrastructure,
    and automatic snaplet resource installation are some of the notable things
    the snaplet infrastructure does for you. For more information on getting
    started with snaplets, check out the [snaplet
    tutorial](/docs/tutorials/snaplets-tutorial).  Also be sure to take a look
    at the [API documentation](http://hackage.haskell.org/package/snap-0.6.0).
    It is written as prose and should function as a more comprehensive,
    api-oriented tutorial.

  - Added session management interface and a built-in snaplet back end based
    on HTTP cookies.

  - Added authentication interface and an implementation snaplet that stores
    users in a flat file.

  - Added a testing API to facilitate debugging on a more granular level.

  - The `Cookie` data structure got two new fields for flagging secure or
    HTTP-only cookies.

  - Changed `Headers` to an opaque data type.

  - Added a new convenience function `expireCookie`.

  - Made logging more flexible by allowing IO-targets.

  - Added `getTemplateFilePath` and `addTemplatePathPrefix` functions to
    Heist.

  - `markdownSplice` no longer requires the template directory as a parameter.
    It uses the directory of the template currently being processed.

  - Added the `templateNames` and `spliceNames` functions to allow the user to
    see what templates and splices are being used by Heist.

## Notable changes

  - `Snap.Types` module renamed to the more appropriate `Snap.Core`.  To ease
    the transition we are leaving `Snap.Types` around for a little while.  But
    it is deprecated and will be going away in the future, so change your code
    now.

  - Renamed `TemplateMonad` to the more appropriate `HeistT`.  Again,
    `TemplateMonad` is still there, but deprecated and will be removed in a
    future release.

  - Deprecated the `Text.Templating.Heist.Splices.Static` module.  The `Cache`
    module now binds both a &lt;cache&gt; and &lt;static&gt; tag.

  - Removed previously deprecated fileServe functions.

  - Deprecated `getRequestBody` in favour of a version that won't DoS your
    server.

  - Removed the deprecated `addCokie` function.

## Bugfixes

  - Added a mime type for JSON.

## Dependency changes

  - Increase the lower version bound on the case-insensitive package from 0.2
    to 0.3.


