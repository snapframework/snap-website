<apply template="page">
  <div class="singlecolumn">
    
    <h2>Snap Quick Start</h2>
    <p>If you haven't already done so, first go to the
      <a href="/download">download</a> page to find out how to install Snap.
      The installation generates an executable called <code>snap</code> that
      you can use to get started with a basic snap project. By
      default, <code>cabal</code> will install executables to
      &ldquo;<code>$HOME/.cabal/bin</code>&rdquo;; the following instructions
      assume that this directory is on your <code>$PATH</code>.</p>

    <p>To set up a new Snap project, run the following commands:</p>

<pre class="code">$ mkdir myproject
$ cd myproject
$ snap init</pre>

    <p>The <code>snap init</code> command creates a template Snap project in
      the current directory. The <code>Main</code> module for this project will
      be created in <code>src/Main.hs</code>.  When you build this project with
      <code>cabal install</code>, an executable is created
      in <code>$HOME/.cabal/bin</code> called
      <code>myproject</code>.  To build and run the example application, run the
      following commands:</p>

<pre class="code">$ cabal install
$ myproject 8000</pre>

    <p>Now point your web browser to localhost:8000; you should see a simple
      response string.</p>
  </div>
</apply>

