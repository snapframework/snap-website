| title: Announcing: Snap Framework v0.2.13
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-10-10T17:10:00+0200
| updated: 2010-10-10T17:10:00+0200
| summary: Release notes for version 0.2.13 of the Snap Framework, (hopefully)
|          the final version of the 0.2 series.

The Snap team is pleased to announce the release of Snap 0.2.13, which should
hopefully be the final Snap version of the 0.2 series. Snap 0.3 will be merged
into the git master branch within the next few hours.

Changes since 0.2.12
=====================

### Improvements:

  - The `fileServe` code now supports [HTTP range
    requests](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.35),
    allowing clients to resume file transfers when downloading from Snap
    servers.


### API Changes:

  - Added a `deleteHeader` function to `snap-core`.


### Bugfixes:

  - Fixed a minor bug in the GZip code which would cause a request to hang if
    the response enumerator caused an iteratee error.

  - Fixed a bug in the `snap` project starter executable regarding "illegal"
    (from Cabal's perspective) characters in the project name.

  - Fixed a potential protocol violation in `snap-server` regarding `HEAD`
    responses; we now strip the response bodies off for incoming `HEAD`
    requests.
