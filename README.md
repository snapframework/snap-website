Snap Website
------------

This is the source code for http://snapframework.com/.

Building
--------

If you wish to work on the Snap website yourself, you will also need to perform
2 extra steps first:

1. Install http://github.com/snapframework/snap-static-pages by hand. This can
   be done by cloning that repository, and then running `cabal install`.

2. Install pandoc, and make sure the executable is visible to your current
   `$PATH`. You can do this by running `cabal install pandoc`, and then making
   sure your `.cabal/bin` directory is in the `$PATH` environment variable. The
   following is usually enough: `export PATH=~/.cabal/bin:$PATH` and you may
   wish to perform this automatically on starting your shell (via `.bashrc` or
   otherwise) if you haven't already.

After that, you can build a development server by running `cabal configure` and
`cabal build`.

