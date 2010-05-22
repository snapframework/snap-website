<apply template="wrap">
<bind tag="subtitle">: Home</bind>
<div class="front-page">


        <div id="splash">

          <div class="slideshow">

            <div class="content">
              <img src="/media/img/splash-img.png" title="Snap Framework" />
              <p class="blurb">
                a web framework for the
                <a href="http://www.haskell.org"><strong>Haskell</strong></a> 
                programming language.
              </p>
            </div>

            <div class="content" style="display: none;">
              <img src="/media/img/splash-img-2.png" title="Snap Framework" />
              <p class="blurb">
                Built for speed from the bottom up. Check out
                some <a href="/benchmarks">benchmarks.</a>
              </p>
            </div>

            <div class="content" style="display: none;">
              <pre class="code code-example">$ cabal update
$ cabal install snap-server
$ mkdir foo; cd foo; snap init</pre>
              <img src="/media/img/splash-img-3.png" title="Snap Framework" />
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
                  <strong>Snap</strong> is a simple web development framework for
                  unix systems, written in the
                  <a href="http://www.haskell.org">Haskell</a> programming
                  language.
                </p>

                <p>
                  Snap is well-documented and has a test suite with a high level
                  of code coverage, but it is <b>early-stage software</b> with
                  still-evolving interfaces. Snap is therefore likely to be most
                  appropriate for early adopters and potential contributors.
                </p>

              </div>
            </div><!--end about-->

            <div id="project-status" class="section right">
              <div class="inner">
                <h2>Snap Features:</h2>

                  <ul>
                    <li>A fast HTTP server library with an optional
                      high-concurrency backend using
                      the <a href="http://software.schmorp.de/pkg/libev.html">libev</a>
                      event loop library</li>
                    <li>A sensible and clean monad for web programming</li>
                    <li>An XML-based templating system for generating HTML</li>
                  </ul>

              </div><!--end inner-->
            </div><!--end project-status-->

          </div>

        </div><!--end content-->
        
</div>
</apply>
