| title: Announcing: Snap Framework v0.5.2
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-07-20T22:07:00+0200
| updated: 2011-07-20T22:07:00+0200
| summary: Release notes for Snap 0.5.2

The Snap team is happy to announce the release of Snap 0.5.2, a bugfix release
of the Snap Framework.

## Bugfixes

  - Fixed [issue #81](https://github.com/snapframework/snap-core/issues/81),
    wherein a response body was not compressed if the Content-Type header field
    had a charset parameter.

  - Fixed [issue #83](https://github.com/snapframework/snap-core/issues/83),
    "Iteratees that throw exceptions aren't handled properly". This one, a
    combination of a bug in Snap and a bug in the
    [enumerator](http://hackage.haskell.org/package/enumerator) (which has been
    fixed, thanks John!), was quite nasty: if the iteratee a user passed into
    `runRequestBody` threw an exception, it could cause the HTTP session to go
    out of frame, making the connection hang. Extra thanks to Bryan O'Sullivan
    for finding this bug and helping us reproduce it.


## New features

  - We now support semicolon-separated query parameters (`?a=x;b=y`) in
    addition to ampersand-separated ones, see
    [http://www.w3.org/TR/1999/REC-html401-19991224/appendix/notes.html#h-B.2.2](`http://www.w3.org/TR/1999/REC-html401-19991224/appendix/notes.html#h-B.2.2`).
    Thanks to Audrey Tang for the patch.


## Functions added

  - Added a function to `Snap.Types`:

~~~~~~ {.haskell}
terminateConnection :: (Exception e, MonadCatchIO m) => e -> m a
terminateConnection = throw . ConnectionTerminatedException . toException
~~~~~~


## Dependency changes

  - We now require [enumerator](http://hackage.haskell.org/package/enumerator)
    0.4.13.1 or above, to fix a problem with `enumerator`'s `catchError`
    function.
