| title: Announcing: Snap Framework v0.4.3
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-06-16T10:48:00-0400
| updated: 2011-06-16T10:48:00-0400
| summary: Release notes for Snap 0.4.3

The Snap team is happy to announce the release of Snap 0.4.3, a small bugfix
release. Here are the changes since 0.4.2:

## Bugfixes

  - Fixed [issue 53](https://github.com/snapframework/snap-core/issues/53),
    which under certain circumstances could cause 100% cpu usage during file
    serving.

  - A workaround for
    [issue 73](https://github.com/snapframework/snap-core/issues/73), although
    that issue is still open and slated to be fixed.


## Datatype changes

  - Added `preServeHook` to `DirectoryConfig` in
    [`Snap.Util.FileServe`](https://github.com/snapframework/snap-core/blob/0.4.3/src/Snap/Util/FileServe.hs#L228).


## Efficiency improvements

  - Improved the Snap monad datatype to flatten the result type, which should
    result in fewer pointer chases in monad-heavy code.
