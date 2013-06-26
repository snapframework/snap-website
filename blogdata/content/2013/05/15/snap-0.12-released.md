| title: Announcing: Snap Framework v0.12
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2013-05-15T20:15:00-0400
| updated:   2013-05-15T20:15:00-0400
| summary: Release notes for Snap 0.12

The Snap team is happy to announce the release of version 0.12 of the Snap
Framework.

## New features

  - Heist now has the ability to reload templates.  Along with this,
    `HeistConfig` now stores template locations instead of templates.  A
    template location is essentially an IO action returning templates.  This
    allows you to have Heist get its templates from a database, over the
    network, etc--anything that can be done from IO.

  - The Heist snaplet now has generic functions that can work with either
    interpreted or compiled templates.  Most applications will choose one of
    either interpreted or compiled templates and not need this new
    functionality.  However, if you are writing a generic snaplet, then you
    probably want it to work no matter which mode the end application uses.
    All you need to do is import the `Snap.Snaplet.Heist.Generic ` module.
    The Heist snaplet defaults to compiled mode.  If you want to use
    interpreted mode, call the `setInterpreted` function in your application
    initializer.

  - It is now possible to reload individual snaplets without reloading the
    whole site.  The snaplet API now includes a function `modifyMaster` that
    you can use to write reload functions for individual snaplets.  The Heist
    snaplet now provides a reloader leveraging this functionality.  We found
    this very useful in allowing us to rapidly iterate when making changes to
    markup in large applications that take a long time to initialize.

## Bugfixes / minor improvements

  - Generalized parts of the compiled splice API to use `RuntimeSplice n a`
    instead of the less general `n a`.  Since RuntimeSplice is a monad
    transformer, this change only requires you to add a call to `lift` in
    places where you have an `n a`.

  - Fixed Heist's `runAttributesRaw` function to do both types of attribute
    parsing.

  - Fixed Heist bug that caused XML templates to be rendered as HTML5.

  - Improve the consistency of the auth snaplet API.

  - Eliminated the inappropriate export of orphan instances.

