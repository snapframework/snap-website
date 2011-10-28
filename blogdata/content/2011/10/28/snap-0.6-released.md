| title: Announcing: Snap Framework v0.6
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2011-10-28T00:25:00-0400
| updated: 2011-10-28T08:16:00+0200
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

  - Added a session management interface and a built-in snaplet back end based
    on HTTP cookies.

  - Added a snaplet interface for user authentication. Out of the box, Snap 0.6
    comes with a simple sample implementation of this user authentication
    interface that stores user information on the filesystem as an example. We
    expect, however, that users choose a more sophisticated implementation of
    this interface corresponding to their choice of data store. Jurriën
    Stutterheim will be releasing an implementation based on HDBC within a few
    days.

  - Added a new testing API to snap-core to make it easier to test Snap
    applications. See the `Snap.Test` module.

  - The `Cookie` data structure got two new fields for flagging secure or
    HTTP-only cookies.

  - Added a new convenience function `expireCookie`.

  - Made logging more flexible by allowing you to specify an arbitrary IO
    action.

  - `heist`: added `getTemplateFilePath` and `addTemplatePathPrefix` functions.

  - `heist`: `markdownSplice` no longer requires the template directory as a parameter.
    It uses the directory of the template currently being processed.

  - `heist`: added the `templateNames` and `spliceNames` functions to allow the user to
    see what templates and splices are being used by Heist.


## API Changes

  - `Headers` from `snap-core` has been changed to an opaque data type, and the
    implementation has changed from `Data.Map` to the more efficient
    `Data.HashMap` from `unordered-containers`. This will break code that
    expects `Headers` to be a type alias for `Map`.

  - The `Snap.Types` module has been renamed to the more appropriate
    `Snap.Core`. `Snap.Types` has been deprecated and made an alias, but will
    be going away with the next few major versions.

  - `heist`: renamed `TemplateMonad` to the more appropriate `HeistT`. Again,
    `TemplateMonad` is still there, but deprecated and will be removed in a
    future release.

  - `heist`: deprecated the `Text.Templating.Heist.Splices.Static` module. The
    `Cache` module now binds both a &lt;cache&gt; and &lt;static&gt; tag.

  - Removed previously deprecated fileServe functions.

  - Deprecated the `getRequestBody` function from `Snap.Core`; it read request
    bodies into memory without specifying a maximum size, meaning that an
    attacker could DoS a server using this function by uploading an arbitrarily
    large request. The function has been replaced by `readRequestBody`, which
    forces you to choose a maximum length.

  - Removed the deprecated `addCookie` function.

  - Added a mime type for JSON.


## Dependency changes

  - Increased the lower version bound on the case-insensitive package from 0.2
    to 0.3.


