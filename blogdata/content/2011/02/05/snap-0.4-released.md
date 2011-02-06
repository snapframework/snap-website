| title: Announcing: Snap Framework v0.4
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-02-06T11:59:00-0500
| updated: 2011-02-06T11:59:00-0500
| summary: Release notes for Snap 0.4, a new major version of the Snap Framework.

The Snap team is proud to announce the release of Snap 0.4, containing a whole
bunch of nifty new features. Here is what we've been up to:


## New Features

  - Heist now uses the brand new
    [xmlhtml](http://hackage.haskell.org/package/xmlhtml) HTML5 parser for
    parsing and rendering templates. This eliminates our dependency on XML and
    makes it possible to include inline javascript/css in templates.  Check out
    Chris Smith's [blog
    post](http://cdsmith.wordpress.com/2011/02/05/html-5-in-haskell/) for more
    information about xmlhtml.

  - Along with the change to xmlhtml, we decided to convert Heist to use
    [Text](http://hackage.haskell.org/package/text) instead of
    [ByteString](http://hackage.haskell.org/package/bytestring). This is a
    backwards-incompatible change which breaks old code, but which we feel is
    the right thing to do.

  - Snap now has support for *file uploads* (!) and the `multipart/form-data`
    content type. Snap's file upload support uses iteratees to stream uploaded
    data and comes with a convenience function to writeuploaded files to a
    temporary directory. We put significant effort into preventing denial of
    service attacks and providing policy controls for things like maximum
    allowable file size, upload timeouts, minimum upload speed, etc.

  - The web server now uses
    [blaze-builder](http://hackage.haskell.org/package/text) in the output
    response body `Enumerator`.  Besides being significantly faster for most
    workloads than `ByteString` enumeration, this allowed us to save several
    copies within the server code, giving us a moderate performance
    improvement. However, this will break any existing code making direct use
    of the output enumerator rather than using convenience functions like
    `writeBS`. We encourage users who are building up large responses out of
    lots of little bytestring chunks to consider switching their code to using
    `Builder` and `writeBuilder` to get a speed boost.

  - The "development mode" of projects built using our
    [snap](http://hackage.haskell.org/package/snap) project starter is now
    quite a bit smarter. The 0.3 version used
    [hint](http://hackage.haskell.org/package/hint) to interpret web handlers
    on-the-fly, but it re-interpreted the code on each request, making keeping
    in-memory state between requests impossible. The development mode now only
    rebuilds the project when files actually change. State will now be
    preserved across requests as long as the project files are not changed on
    disk.

  - Our file serving code has been substantially improved/rewritten. The new
    code can automatically generate stylable/themable directory indexes,
    provides configurable lists of index files, and allows the user to plug in
    dynamic handlers.  These handlers can be used to perform arbitrary
    transformations based on file type or file content on the fly.  The new
    code also correctly handles trailing slashes for relative path resolution.

  - It is now possible for user handlers to modify socket timeouts, using the
   `setTimeout` function. This (finally) makes it possible to have long-running
   request handlers.


## Bugfixes/Improvements

  - Debugging support in
    [snap-core](http://hackage.haskell.org/package/snap-core) is now turned
    *off* by default, as it involves too much of a performance impact. Turning
    on debug support results in a ~25% performance penalty. To turn debugging
    output back on, pass the `-f debug` flag when installing `snap-core`.

  - Bugfix: we no longer log spurious "thread killed" messages in the simple
    backend under HTTP/1.0.

  - Added `catchFinishWith` function.

  - Changed cookie interface to expose cookies in the response as a map for
    easier manipulation.


## An article about Snap

Also, in January, IEEE Internet Computing magazine featured the Snap Framework
in their column "The Functional Web".  Check out the article
[here](http://steve.vinoski.net/blog/2011/01/21/column-on-the-snap-framework/).
