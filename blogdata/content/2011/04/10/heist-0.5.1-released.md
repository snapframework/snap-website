| title: Announcing: Heist v0.5.1
| author: Doug Beardsley <mightybyte@gmail.com>
| published: 2011-04-10T18:50:00-0400
| updated: 2011-04-10T18:50:00-0400
| summary: Release notes for Heist 0.5.1.

The Snap team is proud to announce the release of Heist 0.5.1.  This release
is a direct result of experience gained in real-world use.  We really like the
way Heist is shaping up, and we think you will too.


## New Features

  - Head merging.  Now you can use <head> at any place in any template as long
    as it is a descendent of <html>.  All occurrences of <head> will be moved
    to the top of the page and concatenated in the order they were encountered.
    This improves modularity by allowing you to include css and javascript
    files in the templates where they are used rather than in a top-level
    template.

  - A <cache> tag.  This is similar to <static>, but reloads the content when
    the specified time to life expires.

  - A number of new functions for common patterns of use.  This includes
    textSplice, mapSplices, and the runChildren variants.

  - The <bind> tag now accepts paramaters similar to <apply>.


## Bugfixes/Improvements

  - Fixed a bug in lookupTemplate that caused callTemplate and evalTemplate to
    mistakenly fail to find templates in one subdirectory when the current
    context was a different subdirectory.

  - Corrected mistakes in haddock docs.

