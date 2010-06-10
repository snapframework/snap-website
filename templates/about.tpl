<apply template="page">
  <bind tag="subtitle">: About</bind>
  <static>
    <div id="about" class="section left">
      <div class="inner">
        <h2>What is the Snap Framework?</h2>

        <p><img class="small-logo" src="/media/img/logo-small.png" /><strong>Snap</strong> is a simple web development framework for unix
          systems, written in the <a href="http://www.haskell.org">Haskell</a>
          programming language.
        </p>

        <p>
          <strong>Snap</strong> aims to be the <i>de facto</i> web toolkit
          for Haskell, on the basis of:
          <ul>
            <li>High performance</li>
            <li>High design standards</li>
            <li>Simplicity and ease of use, even for Haskell beginners</li>
            <li>Excellent documentation</li>
            <li>Robustness and high test coverage</li>
          </ul>
        </p>

        <h2>What is the project's status?</h2>
        <p>
          The first developer prerelease of the Snap framework is available
          now. It contains only the Snap core system, namely:
          <ul>
            <li>A fast HTTP server library with an optional high-concurrency
                backend using
                the <a href="http://software.schmorp.de/pkg/libev.html">libev</a>
                event loop library.</li>
            <li>A sensible and clean monad for web programming.</li>
            <li>an XML-based templating system for generating HTML that allows
                you to bind Haskell functionality to XML tags without getting
                PHP-style tag soup all over your pants</li>
          </ul>
        </p>

        <p>
          Higher-level facilities for building web applications (like
          user/session management, component interfaces, data modeling, etc.)
          are planned but not yet implemented, so this release will mostly be
          of interest to those who:
          <ul>
            <li>
              need a fast and minimal HTTP API at roughly the same level of
              abstraction as Java servlets, or
            </li>
            <li>
              are interested in contributing to the Snap Framework
              project.
            </li>
          </ul>
        </p>
        <p>
          Snap runs on *nix platforms; it has been tested on Linux and Mac OSX
          Snow Leopard.  Windows support was added more recently, but it not
          as well-tested.
        </p>

      </div>
    </div><!--end about-->

    <div id="project-status" class="section right">
      <div class="inner">
        <div id="developers"  >
          <h2>Who's involved?</h2>
          <p>
            <strong><a href="http://gregorycollins.net/">Gregory
            Collins</a></strong> is a programmer from Toronto, Canada. He holds
            an MSc in Computer Science from Yale University and he works
            at <a title="Sysomos - Social Media Monitoring and Analytics"
                  href="http://www.sysomos.com/">Sysomos</a>, a social media
            analytics startup.
          </p>
          <p>
            <strong><a href="http://softwaresimply.blogspot.com/">Doug
            Beardsley</a></strong> loves Haskell and has been playing with
            Haskell web development since 2008.  He currently works for a
            finance company doing Haskell development.
          </p>
          <p>
            <strong><a href="http://rfrn.org/~shu">Shu-yu Guo</a></strong>
            is a PhD student in programming languages and compilers
            at <a href="http://www.ucla.edu">UCLA</a>. He is also an
            unaccomplished linguist and a professional StarCraft
            enthusiast.
          </p>
          <p>

            <strong><a href="http://james-sanders.com">James
            Sanders</a></strong> has been in the web development game for over
            six years.  He works
            at <a href="http://thisismedium.com/">Medium</a>.  You can e-mail
            him
            at <a href="mailto:jimmyjazz14@gmail.com">jimmyjazz14@gmail.com</a>.
          </p>
        </div><!--end developers-->
        <div>
          <h2>Contacting us</h2>
          <ul>
            <li><b>IRC:</b> You can discuss snap in the
              <a target="_blank"
                 href="http://webchat.freenode.net/?channels=snapframework&amp;uio=d4"
                 ><tt>#snapframework</tt></a>
              channel on the <a href="http://freenode.net/">freenode</a> IRC
              network.</li>
                                
            <li><b>Email:</b> join
              our <a href="http://mailman-mail5.webfaction.com/listinfo/snap">mailing
              list.</a></li>
          </ul>
        </div><!--end mailing list -->
      </div><!--end inner-->
    </div><!--end project-status-->
  </static>
</apply>
