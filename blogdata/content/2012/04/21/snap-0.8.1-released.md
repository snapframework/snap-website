| title: Announcing: Snap Framework v0.8.1
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2012-04-21T17:27:00+0200
| updated:   2012-04-21T17:27:00+0200
| summary: Release notes for Snap 0.8.1

The Snap team would like to announce the release of version 0.8.1 of the Snap
Framework.

## New features

  - `Snap.Http.Server.Config` now has a new `extendedCommandLineConfig`
    function, which should make it easier to add your own command-line flags
    while still re-using most of the Snap command-line processing logic.


## Bugfixes / minor improvements

  - Added a MIME type for favicons to `Snap.Util.FileServe`.

  - Switched the internals of the tries used in our routing tables from
    `Data.Map` to a hashmap.

  - Minor fixes to the snap tutorial projects.


## Dependencies

  - All Snap dependencies have been bumped to work with the latest Hackage
    packages.
