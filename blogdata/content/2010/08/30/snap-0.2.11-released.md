| title: Announcing: Snap Framework v0.2.11
| author: Gregory Collins <greg@gregorycollins.net>
| published: 2010-08-30T20:06:01-0400
| updated: 2010-08-31T14:28:39-0400
| summary: Release notes for version 0.2.11 of the Snap Framework.

Hi all,

The Snap team is pleased to announce the release of Snap 0.2.11. Most of the
changes since 0.2.9 are bugfixes and testing improvements.

Changes since 0.2.9
=====================

### Bugfixes:

 - Fix an iteratee bug in
   [`enumLBS`](/docs/latest/snap-core/Snap-Iteratee.html#v%3AenumLBS) regarding
   incorrect eof handling.

 - Normally when enumerating a file (and not using `sendfile()`), Snap uses
   `mmap()` to get the file data, saving a copy. Enumerating a large file could
   exhaust address space on 32-bit systems, however, so for files larger than
   40MB Snap will now use `enumHandle`.

 - Fixes to support `iteratee-0.3.6`.

 - A fix for the "`error.log` spamming bug" (wherein snap would spew thousands
   of spurious error messages to the error log upon exit).

 - Snap now handles `Expect: 100-continue` properly.

### Improvements:

 - We are now exporting a new function
   [`setResponseCode`](/docs/latest/snap-core/Snap-Types.html#v%3AsetResponseCode)
   to set the reponse status code and message together using a lookup table,
   rather than forcing you to input the status message yourself.

 - Added a 
   [`getCookie`](/docs/latest/snap-core/Snap-Types.html#v%3AgetCookie)
   function.

 - Architecture-specific hash function is now selected via template haskell.

 - Tweaks to the date thread to prevent blocking when computing the current
   time.
