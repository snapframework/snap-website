<apply template="page">
<h2>Snap Quick Start</h2>
<p>If you haven't already done so, first go to the <a
href="/download">download</a> page to find out how to install Snap.
The installation generates an executable <i>snap</i> that you can use
to get started with a basic snap project.  To use it, run the
following commands:</p>

<pre>
  mkdir myproject
  cd myproject
  snap init
</pre>

<p>That last command creates a template Snap project in the current
directory.  (If it doesn't run for you, then try
<i>~/.cabal/bin/snap</i> instead.)  The source code for this project
will be in <i>src/Main.hs</i>.  When you build this project with
<i>cabal install</i>, an executable is created called
<i>myproject</i>.  When you run it, the example site will be served on
port 8080.  To build and run the example application, run the
following commands:</p>

<pre>
  cabal install
  ~/.cabal/bin/myproject
</pre>

<p>Now point your web browser to localhost:8000.  And you should see
a simple response string.</p>
</apply>

