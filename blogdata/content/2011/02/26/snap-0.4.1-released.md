| title: Announcing: Snap Framework v0.4.1
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2011-02-26T17:40:00+0100
| updated: 2011-02-06T17:40:00+0100
| summary: Release notes for Snap 0.4.1

The Snap team is proud to announce the release of Snap 0.4.1, containing
bugfixes and efficiency improvements. Here are the changes since 0.4.0:

## Bugfixes

  - The `MonadCatchIO` instance for the `Snap` monad was broken (this was [bug
  #48](https://github.com/snapframework/snap-core/issues/closed#issue/48) on
  the issue tracker), causing exceptions to leak outside of catch blocks. This
  is now fixed. Thanks to Bryan O'Sullivan for reporting this.

  - Fixed several space leaks in file upload support, one of which was fixed by
  a new release of
  [enumerator](http://hackage.haskell.org/package/enumerator). Thanks to John
  Millikin for fixing the space leak in `Iteratee` bind. [CPS is
  hard!](http://www.serpentine.com/blog/2011/02/25/cps-is-great-cps-is-terrible/)

  - Our code which generated "`Set-Cookie`" headers from the `Cookie` list
  stored in the `Response` object had the unfortunate side-effect of blowing
  away any "`Set-Cookie`" headers users put into the response by hand.

  
## Efficiency improvements

  - At the suggestion of [Jasper Van der Jeugt](http://jaspervdj.be/), changed
    the string match algorithm in our file upload code from
    [Knuth-Morris-Pratt](http://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm)
    to
    [Boyer-Moore-Horspool](http://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm). File
    uploads now use much less CPU time, thanks Jasper!

  - In `snap-server`, eliminated an unnecessary copy in our socket `recv` calls.


## Upcoming talk

[Gregory Collins](http://gregorycollins.net/) will be speaking at [QCon
London](http://qconlondon.com/london-2011/tracks/show_track.jsp?trackOID=424)
on Friday, March 11 about "High-performance web applications in Haskell". The
talk is not specifically about the Snap Framework --- although Snap will be
discussed --- but instead will be about why Haskell is a great fit for
high-performance web applications, with a nod to projects like
[Happstack](http://happstack.com/),
[Warp](http://hackage.haskell.org/package/warp), and
[Yesod](http://docs.yesodweb.com/).
