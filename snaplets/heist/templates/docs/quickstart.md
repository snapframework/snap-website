## Snap Quick Start Guide

If you haven't already done so, first go to the [download](/download) page to
find out how to install Snap.  The installation generates an executable called
`snap` that you can use to get started with a basic snap project.

Our first step is to complete the installation process by adding `snap` to our `PATH`.  
This step depends on your exact system configuration.  Below are the commands for the 
default configurations of the haskell platform on Linux, Mac, and Windows.

On Linux, the command is:
~~~~~~ {.shell}
PATH=$HOME/.cabal/bin:$PATH
~~~~~~

On MacOS, the command is:
~~~~~ {.shell
PATH=$HOME/Library/Haskell/bin:$PATH
~~~~~

On Windows (with cygwin), the command is:
~~~~~ {.shell}
PATH=/cygdrive/c/Users/yourusername/AppData/Roaming/cabal/bin:$PATH
~~~~~
(you must replace yourusername with your actual user name.)

Add the appropriate command to your `~/.bashrc`, then run `source ~/.bashrc`. The following
instructions assume that `$HOME/.cabal/bin` is on your `$PATH`. 

To set up a new Snap project, run the following commands:

~~~~~~ {.shell}
$ cabal install snap snap-templates
$ mkdir myproject
$ cd myproject
$ snap init barebones
~~~~~~

The `snap init` command creates a template Snap project in the current
directory.  If you run `snap init` with no extra argument, it will create a
larger application demonstrating the use of Snap's higher-level features such
as the auth snaplet, heist, etc.  You can also do `snap init -h` to see help
about other project templates that are available.  For now we'll focus on the
barebones project.

The `Main` module for this project will be created in `src/Main.hs`.  When you
build this project with `cabal install`, an executable is created in
`$HOME/.cabal/bin` called `myproject`.  To build and run the example
application, run the following commands:

~~~~~~ {.shell}
$ cabal install
$ myproject -p 8000
~~~~~~

Now point your web browser to [http://localhost:8000/](http://localhost:8000/);
you should see a simple response string.

To activate dynamic recompilation in your project, rebuild your application
with "cabal clean; cabal install -fdevelopment".  This won't work with the
barebones project that we created above.  You have to create your project with
"snap init" instead.

For more information, continue on to the [Snap API
tutorial](tutorials/snap-api) or take a look at the [Snap
reference documentation](/docs).


