<bind tag="subtitle">: Download</bind>
<apply template="page">
  <static>
    <div class="section left">
      <div class="inner">
        <h2>Installing Snap</h2>
        <p>Snap is written in the <a href="http://www.haskell.org/">Haskell</a>
          programming language, and requires
          the <a href="http://www.haskell.org/ghc/">GHC</a> Haskell
          compiler.</p>

        <p>The easiest way to get Snap is with Haskell's
          <a href="http://www.haskell.org/cabal/">Cabal</a> package manager. If
          you already have Cabal set up, then getting Snap should be as simple
          as running two commands:</p>
        <pre class="code">$ cabal update
$ cabal install snap-server</pre>

        <p>If you don't have <tt>GHC</tt> and <tt>cabal</tt> installed, the
          easiest way to get them is with
          the <a href="http://hackage.haskell.org/platform/">Haskell Platform</a>
          binary installer.</p>

        <p>After Snap is installed, check out the <a href="/docs/quickstart">quick
            start</a> for instructions on getting your project started.</p>

        <h2>Buildbot</h2>
        <p>We have set up
           a <a href="http://buildbot.snapframework.com/">continuous
           integration server</a> to track the progress of our development. The
           buildbot also builds an up-to-date version of the Snap haddocks if
           you want to follow along with our git master branch.</p>

      </div>
    </div>

    <div class="section right">
      <div class="inner">
        <h2>Snap Packages</h2>

        <p class="foundabug"><b>Found a bug in Snap? Please visit our <a
        href="http://github.com/snapframework/snap-core/issues">issue
        tracker.</a></b></p>

        <p>Snap is made up of a collection of separate packages:</p>
        <div id="packages">
          <dl>
            <dt>snap-core</dt>
            <dd>Core type definitions (<tt>Snap</tt> monad, HTTP types, etc) and
              utilities for web
              handlers. <br/><span class="linklist">
                [ <a href="/docs/latest/snap-core/index.html"
                     >api docs</a> |
                <a href="http://hackage.haskell.org/package/snap-core"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap-core"
                   >github</a> |
                <a href="/docs/snap-core-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>

            <dt>snap-server</dt>
            <dd>An iteratee-based HTTP server library, which runs <tt>Snap</tt> web
              handlers. <br/><span class="linklist">
                [ <a href="/docs/latest/snap-server/index.html"
                     >api docs</a> |
                <a href="http://hackage.haskell.org/package/snap-server"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap-server"
                   >github</a> |
                <a href="/docs/snap-server-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>


            <dt>heist</dt>
            <dd>An xhtml-based templating engine, allowing Haskell functions to be
              bound to XML tags. <br/><span class="linklist">
                [ <a href="/docs/latest/heist/index.html" >api docs</a> |
                <a href="http://hackage.haskell.org/package/heist"
                   >hackage</a> |
                <a href="http://github.com/snapframework/heist"
                   >github</a> |
                <a href="/docs/heist-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>


            <dt>snap</dt>
            <dd>Snap project starter plus hint-based web handler engine
                (upcoming for 0.3, works with the 0.3 branch of
                <a href="http://github.com/snapframework/snap-core">snap-core</a>)<br/><span class="linklist">
                [ <a href="http://github.com/snapframework/snap" >github</a>
                ]</span></dd>

            <dt>snap-website </dt>
            <dd>The source code for this website is a good example of
              Snap in action. <br/><span class="linklist">
                [ <a href="http://github.com/snapframework/snap-website"
                     >github</a> ]</span></dd>
          </dl>
        </div>
      </div>
    </div>
  </static>
</apply>
