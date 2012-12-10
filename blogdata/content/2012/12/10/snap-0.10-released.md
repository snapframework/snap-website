| title: Announcing: Snap Framework v0.10
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2012-12-10T09:20:00-0400
| updated:   2012-12-10T09:20:00-0400
| summary: Release notes for Snap 0.10

The Snap team is happy to announce the release of version 0.10 of the Snap
Framework.  This release updates the snap package to support Heist 0.10.  Our
[migration
guide](https://github.com/snapframework/heist/wiki/Migrating-Snap-Applications-to-Heist-0.10)
has more detailed information about migrating your applications to 0.10, 

## New features

  - A part of supporting Heist 0.10 includes adding four new functions
    `cRender`, `cRenderAs`, `cHeistServe`, and `cHeistServeSingle` for
    rendering and serving compiled templates.  These functions are much faster
    than the old versions that use interpreted templates.

  - heistServe ignores templates with names beginning with an underscore.
    This allows you to define "private" templates for parts of a page and
    ensure that they will not be served.

  - We have a new module Snap.Snaplet.Test that mirrors the test module from
    snap-core and makes it easier to test your snaplets.

  - Removed the SnapletHeist wrapper for Heist computations in a Handler
    monad.

  - Switched to Edward Kmett's new
    [lens](http://hackage.haskell.org/package/lens) library.  This changes the
    order that lens composition uses, so you should change (f . g) to (g . f).
    The nice thing about this is you no longer have to import the (.) function
    from Control.Category and hide it from Prelude.

  - Added password reset functionality to the auth snaplet.

## Bugfixes / minor improvements

  - Added missing instances to Snaplet for Functor, Foldable, Traversable, and
    Comonad.

  - A number of documentation updates

## Dependencies

  - Increased stm upper bound to include 2.4.*.

  - Expanded lower bound for pwstore-fast to include 2.2.
