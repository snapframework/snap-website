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
$ cabal install snap snap-templates
$ mkdir foo; cd foo; snap init
</pre>

        <p>If you don't have <tt>GHC</tt> and <tt>cabal</tt> installed, the
          easiest way to get them is with
          the <a href="http://hackage.haskell.org/platform/">Haskell Platform</a>
          binary installer.</p>

        <p>After Snap is installed, check out the <a href="/docs/quickstart">quick
            start</a> for instructions on getting your project started.</p>

        <h2>Buildbot</h2>
        <p>We have set up
           <a href="https://travis-ci.org/snapframework">continuous
           integration</a> to track the progress of our development.</p>

        <table class="travis">
          <tr>
            <th>Package</th>
            <th>Status</th>
          </tr>
          <tr>
            <td>snap</td>
            <td>
              <a href="https://travis-ci.org/snapframework/snap"><img src="https://travis-ci.org/snapframework/snap.svg?branch=master"/></a></td>
          </tr>
          <tr>
            <td>snap-server</td>
            <td><a href="https://travis-ci.org/snapframework/snap-server"><img src="https://travis-ci.org/snapframework/snap-server.svg?branch=master"/></a></td>
          </tr>
          <tr>
            <td>snap-core</td>
            <td><a href="https://travis-ci.org/snapframework/snap-core"><img src="https://travis-ci.org/snapframework/snap-core.svg?branch=master"/></a></td>
          </tr>
          <tr>
            <td>io-streams-haproxy</td>
            <td><a href="https://travis-ci.org/snapframework/io-streams-haproxy"><img src="https://travis-ci.org/snapframework/io-streams-haproxy.svg?branch=master"/></a></td>
          </tr>
          <tr>
            <td>heist</td>
            <td><a href="https://travis-ci.org/snapframework/heist"><img src="https://travis-ci.org/snapframework/heist.svg?branch=master"/></a></td>
          </tr>
          <tr>
            <td>xmlhtml</td>
            <td><a href="https://travis-ci.org/snapframework/xmlhtml"><img src="https://travis-ci.org/snapframework/xmlhtml.svg?branch=master"/></a></td>
          </tr>
        </table>

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
            <dd>A simple web server API containing core type definitions
              (<tt>Snap</tt> monad, HTTP types, etc) and utilities for web
              handlers. <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/snap-core"
                     >api docs</a> |
                <a href="http://hackage.haskell.org/package/snap-core"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap-core"
                   >github</a> |
                <a href="/docs/snap-core-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>

            <dt>snap-server</dt>
            <dd>A web server based on io-streams, which runs <tt>Snap</tt> web
              handlers. <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/snap-server"
                     >api docs</a> |
                <a href="http://hackage.haskell.org/package/snap-server"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap-server"
                   >github</a> |
                <a href="/docs/snap-server-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>


            <dt>snap</dt>
            <dd>Umbrella project pulling together Snap's component libraries
                into a coherent framework.  <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/snap"
                     >api docs</a> |
                <a href="http://hackage.haskell.org/package/snap"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap"
                   >github</a> ]</span></dd>


            <dt>snap-templates</dt>
            <dd>A package containing a project template generator and a few simple project 
                templates.  <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/snap-templates"
                   >hackage</a> |
                <a href="http://github.com/snapframework/snap-templates"
                   >github</a> ]</span></dd>


            <dt>heist</dt>
            <dd>An HTML-based templating engine, allowing Haskell functions to be
              bound to tags. <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/heist" >api docs</a> |
                <a href="http://hackage.haskell.org/package/heist"
                   >hackage</a> |
                <a href="http://github.com/snapframework/heist"
                   >github</a> |
                <a href="/docs/heist-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>


            <dt>xmlhtml</dt>
            <dd>A hybrid XML/HTML5 parsing and rendering library written
              specifically for Heist. <br/><span class="linklist">
                [ <a href="http://hackage.haskell.org/package/xmlhtml" >api docs</a> |
                <a href="http://hackage.haskell.org/package/xmlhtml"
                   >hackage</a> |
                <a href="http://github.com/snapframework/xmlhtml"
                   >github</a> |
                <a href="/docs/xmlhtml-hpc/hpc_index.html"
                   >test coverage report</a> ]</span></dd>


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
