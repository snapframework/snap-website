| title: Announcing: Snap Framework v0.4
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-02-05T19:15:00+0100
| updated: 2011-02-05T19:15:00+0100
| summary: Release notes for Snap 0.4, a new major version of the Snap Framework.

The Snap team is proud to announce the release of Snap 0.4.  Here is what
we've been up to:


## New Features

  - Heist now uses the xmlhtml library for parsing and rendering templates.
    This makes it possible to include inline javascript/css in templates.
    Check out Chris Smith's [blog
post](http://cdsmith.wordpress.com/2011/02/05/html-5-in-haskell/) for more
information.

  - Snap now has support for file uploads and the multipart/form-data content
    type.  We put significant effort into preventing denial of service attacks
    and providing built-in policy controls for things like maximum allowable
    file size, upload timeouts, minimum upload speed, etc.

  - The web server now uses blaze-builder to generate responses.  This gives
    the server a significant performance improvement.  However, this will
    break any existing code making direct use of the output enumerator.

  - Dynamic recompilation now only rebuilds the project when files actually
    change.  Rebuilds automatically destroy any in-memory server-side state
    not persisted to disk.  Previously, this happened on every request, which
    made stateful websites essentially unusable in development mode.  Now
    state will be preserved across requests as long as the project files are
    not changed on disk.

  - File serving has been improved to automatically generate directory
    indexes.  The new code also provides configurable lists of index files,
    pluggable auto-generated indexes with a nice default look-and-feel, and
    allows the user to write dynamic handlers.  These handlers can be used to
    modify trailing slashes or perform arbitrary transformations based on file
    type or file content on the fly.

## Bugfixes/Improvements

  - We no longer log "thread killed" messages unnecessarily in the simple
    backend.

  - Added an API function for modifying socket timeouts.  This makes it
    possible to have long-running request handlers.

  - Added catchFinishWith function.

  - Changed cookie interface to expose cookies in the response as a map for
    easier manipulation.

Also, in January, IEEE Internet Computing magazine featured the Snap Framework
in their column "The Functional Web".  Check out the article
[here](http://steve.vinoski.net/blog/2011/01/21/column-on-the-snap-framework/).

