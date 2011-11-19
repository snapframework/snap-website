<apply template="wrap">
<bind tag="subtitle">: Home</bind>
<div class="front-page">


        <div id="splash">

          <div class="slideshow">

            <div class="content">
              <img width="960" height="282"
                   src="/media/img/splash-img.png" title="Snap Framework" />
              <p class="blurb">
                a web framework for the
                <a href="http://www.haskell.org"><strong>Haskell</strong></a> 
                programming language.
              </p>
            </div>

            <div class="content" style="display: none;">
              <img width="960" height="282"
                   src="/media/img/splash-img-2.png" title="Snap Framework" />
              <p class="blurb">
                Built for speed from the bottom up. Check out
                some <a href="/benchmarks">benchmarks.</a>
              </p>
            </div>

            <div class="content" style="display: none;">
              <pre class="code code-example">$ cabal update
$ cabal install snap
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

                  <strong>Snap</strong> is a simple web development framework for
                  unix systems, written in the
                  <a href="http://www.haskell.org">Haskell</a> programming
                  language.  Snap has a high level of test coverage and is
                  well-documented.  Features include:

                </p>

                <ul>
                  <li>A fast HTTP server library with an optional
                    high-concurrency backend using
                    the <a href="http://software.schmorp.de/pkg/libev.html">libev</a>
                    event loop library</li>
                  <li>A sensible and clean monad for web programming</li>
                  <li>An HTML-based templating system for generating pages</li>
                </ul>

              </div>
            </div><!--end about-->

            <div id="project-status" class="section right">
              <div class="inner">
                <h2>New: Snap 0.6</h2>
                <p>
                  Snap 0.6 is an exciting new release which offers a completely
                  redesigned extension system called
                  <a href="/docs/tutorials/snaplets-tutorial">"snaplets"</a>.
                  It is now easier then ever before to write, share, reuse
                  code, and get your applications up and running in no time.
                </p>
                <p>
                  <a href="/download">Install</a> Snap 0.6 now, check out the
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
