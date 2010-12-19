## Snap Quick Start Guide

If you haven't already done so, first go to the [download](/download) page to
find out how to install Snap.  The installation generates an executable called
`snap` that you can use to get started with a basic snap project. By default,
`cabal` will install executables to "`$HOME/.cabal/bin`".  To add `cabal` to your `$PATH`, add 

~~~~~~ {.shell}
PATH=$PATH:$HOME/.cabal/bin
~~~~~~

to your `~/.bashrc` and run `source ~/.bashrc`. The following
instructions assume that `$HOME/.cabal/bin` is on your `$PATH`. 

Note also that before you start, you should have a development version of the
`expat` library on your system; for instance, on Debian you would run:

~~~~~~ {.shell}
$ sudo apt-get install libexpat-dev
~~~~~~

and on Ubuntu you'd run:

~~~~~~ {.shell}
$ sudo apt-get install libexpat1-dev
~~~~~~

Mac OSX should already have the required C libraries installed. On Windows, see
[this article by Jacob
Stanley](http://jystic.com/2010/10/07/installing-heist-and-hexpat-on-windows/)
for more information.

To set up a new Snap project, run the following commands:

~~~~~~ {.shell}
$ mkdir myproject
$ cd myproject
$ snap init
~~~~~~

The `snap init` command creates a template Snap project in the current
directory. The `Main` module for this project will be created in `src/Main.hs`.
When you build this project with `cabal install`, an executable is created in
`$HOME/.cabal/bin` called `myproject`.  To build and run the example
application, run the following commands:

~~~~~~ {.shell}
$ cabal install
$ myproject -p 8000
~~~~~~

Now point your web browser to [http://localhost:8000/](http://localhost:8000/);
you should see a simple response string.

For more information, continue on to the [Snap API
tutorial](tutorials/snap-api) or take a look at the [Snap
reference documentation](/docs).


