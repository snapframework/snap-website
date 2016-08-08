<apply template="wrap">
<bind tag="subtitle">: Home</bind>
<div class="front-page">


        <div id="splash">

          <div class="slideshow">

            <div class="content">
              <img width="960" height="282"
                   src="/media/img/splash-img.png" title="Snap Framework" />
              <p class="blurb">
                A web framework for the
                <a href="http://www.haskell.org"><strong>Haskell</strong></a> 
                programming language.
              </p>
            </div>

            <div class="content" style="display: none;">
              <img width="960" height="282"
                   src="/media/img/splash-img-2.png" title="Snap Framework" />
              <p class="blurb">
                Built for speed, simplicity and stability. Snap supports ghc versions from 7.4 through 8.0.
              </p>
            </div>

            <div class="content" style="display: none;">
              <pre class="code code-example">$ cabal update
$ cabal install snap snap-templates
$ mkdir foo; cd foo; snap init</pre>
              <img width="960" height="282"
                   src="/media/img/splash-img-3.png" title="Snap Framework" />
              <p class="blurb">Snap is <a href="/download">quick and easy to
              install</a>.
              </p>
            </div>


          </div>

          <div id="pager"> </div>

        </div>


        <div id="triptych">
          <div class="triptych-content" >
            <h2>Downloads</h2>
            <h2>Documentation</h2>
            <h2>About</h2>

          </div>
        </div>


        <div id="content">
          <div class="newspaper">
            <div id="about" class="section left">
              <div class="inner">
                <h2>What is Snap?</h2>

                <p>

                  <strong>Snap</strong> is a simple web development framework
                  for unix systems, written in the <a
                  href="http://www.haskell.org">Haskell</a> programming
                  language. Snap has a [high level of test
                  coverage](https://snapframework.github.io/snap-code-coverage/snap-server/hpc-ghc-8.0.1/hpc_index.html)
                  and is well-documented. Features include:

                </p>

                <ul class="compact">
                  <li>A fast HTTP server library</li>
                  <li>A sensible and clean monad for web programming</li>
                  <li>An HTML-based templating system for generating pages</li>
                </ul>

              </div>
            </div><!--end about-->

            <div id="project-status" class="section right">
              <div class="inner">
                <h2>Snaplets</h2>
                <p>
                  Snap includes an optional system for building reusable pieces
                  web functionality called <strong><a
                  href="/docs/tutorials/snaplets-tutorial">&#8220;snaplets&#8221;</a></strong>.
                  Snaplets make it easy to share and reuse common code across
                  multiple web apps. The default snaplets let you get a
                  full-featured web application up and running in no time.
                </p>
                <p>
                  <strong><a href="/download">Install</a></strong> Snap now, check out the
                  <a href="/snaplets">available snaplets</a>, and
                  start <a href="/docs/tutorials/snaplets-tutorial">writing</a>
                  your own!
                </p>
              </div><!--end inner-->
            </div><!--end project-status-->

          </div>

        </div><!--end content-->

</div>
</apply>
