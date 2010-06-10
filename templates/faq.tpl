<apply template="page">
  <bind tag="subtitle">: FAQ</bind>
  <static>
  <div class="singlecolumn">
    <h2>Frequently Asked Questions</h2>
    <ol>
      <li><a href="#live-sites">Is anyone using Snap in production?</a></li>
      <li><a href="#high-level">Where's the high-level functionality?</a></li>
      <li><a href="#install">Why can't I install Snap?</a></li>
      <li><a href="#help">How can I help?</a></li>
    </ol>

    <h3 id="live-sites">Is anyone using Snap in production?</h3>

    <p>Yes!  Here is a list of sites that we know of that use Snap.  Let us
    know if you know of others</p>

    <ul>
      <li><a href="http://snapframework.com">http://snapframework.com</a> (this site)</li>
      <li><a href="http://darcsden.com">http://darcsden.com</a></li>
    </ul>

    <h3 id="high-level">Where's the high-level functionality?</h3>

    <p>Our goal is for Snap to be a very fast, stable, <em>high-level</em> web
    framework at or above the same level of abstraction as frameworks like Ruby
    on Rails, Django, etc.  During early planning and development we concluded
    that to accomplish this goal we needed to build our own web server and API
    to interface with it.  This was an unanticipated detour, and we will resume
    working on higher-level functionality when the core has stabilized.</p>

    <h3 id="install">Why can't I install Snap?</h3>

    <p>First, make sure you have "$HOME/.cabal/bin" at the beginning of your
    path.</p>
    
    <p>You may have an old version of Cabal.  Try running "<code>cabal update
    &amp;&amp; cabal install Cabal</code>".  After this, "<code>cabal
    --version</code>" should say that you're using version 1.8.0.4 of the
    Cabal library or higher.  After you do this try installing Snap again.</p>

    <p>If that didn't work, and you're getting an error that mentions
    monads-fd or transformers, try running "<code>cabal install --reinstall
    monads-fd</code>" (or transformers).</p>

    <h3 id="help">How can I help?</h3>

    <h5>Use Snap to build real websites.</h5>

    <p>This is perhaps the best way to help.  Let us know what issues you
    encounter and work on fixing the ones you care about most.  If you are
    unable to fix a problem, you can still help by writing an automated test
    case that detects the problem.</p>

    <h5>Develop automated memory leak and performance regression testing.</h5>

    <p>Currently our top priority is working out correctness and performance
    issues in the server.  We have a CI build server that automatically runs
    all our test cases, but we don't have an automated system to test for
    performance and memory leaks.  This would be a very helpful addition.</p>

    <h5>Improve test cases and code coverage.</h5>

    <p>While not an exotic task, expanding our test suite can contribute
    significantly to the stability of the project.</p>

    <h5>Improve documentation and tutorials.</h5>

    <p>It's easy for documentation to get out of date.  We try to keep it
    up-to-date, but we can always use more eyes to catch things that slip
    through the cracks.</p>

  </div>
  </static>
</apply>
