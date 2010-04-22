<snap:apply template="page">
<h2>Snap Framework 0.1.1</h2>

<p>This is the first developer prerelease of the Snap framework.  Snap is
a simple and fast web development framework and server written in
Haskell. For more information or to download the latest version, you
can visit the Snap project website at http://snapframework.com/.</p>


<h3>Snap Status and Features</h3>

This developer prerelease contains only the Snap core system, namely:

<ul>
  <li>a high-speed HTTP server, with an optional high-concurrency</li>
  <li>backend using the <a href="http://software.schmorp.de/pkg/libev.html">libev</a> library</li>
  <li>a sensible and clean monad for web programming</li>
  <li>an xml-based templating system for generating HTML based on
    <a href="http://expat.sourceforge.net/">expat</a> (via
    <a href="http://hackage.haskell.org/package/hexpat">expat</a>) that allows you to
    bind Haskell functionality to XML tags without getting PHP-style tag soup all over your pants</li>
</ul>

<p>Snap currently only runs on Unix platforms; it has been tested on
Linux and Mac OSX Snow Leopard.</p>


<h3>Snap Philosophy</h3>

<p>Snap aims to be the *de facto* web toolkit for Haskell, on the basis
of:</p>

<ul>
  <li>High performance</li>

  <li>High design standards</li>

  <li>Simplicity and ease of use, even for Haskell beginners</li>

  <li>Excellent documentation</li>

  <li>Robustness and high test coverage</li>
</ul>

<h3>Snap Roadmap</h3>

<p>Where are we going?</p>

<ol>
<li> First prerelease: HTTP server, monad, template system</li>

<li> Second prerelease: component system with a collection of useful stock modules (called "Snaplets") for things like user and session management, caching, an administrative interface, etc.</li>

<li> Third prerelease: where we figure out what to do about data access</li>
</ol>
</snap:apply>
