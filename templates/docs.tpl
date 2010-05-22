<bind tag="subtitle">: Docs</bind>
<apply template="page">
  <static>
    <div id="about" class="section left">
      <div class="inner">
        <h2>Tutorials</h2>
        <div id="docdls">
          <dl>
            <dt><a href="docs/quickstart">Quick Start</a></dt>
            <dd>A guide to getting Snap installed.</dd>

            <dt><a href="docs/tutorials/snap-api">Snap API Introduction</a></dt>
            <dd>An in-depth tutorial on the Snap API. Covers installation,
              iteratee I/O, the &ldquo;snap&rdquo; command-line tool, the Snap
              monad, and URL routing.</dd>

            <dt><a href="docs/tutorials/heist">Heist Template Tutorial</a></dt>
            <dd>A tutorial for the Heist xhtml templating library.</dd>
          </dl>
        </div>
      </div>

      <div class="inner">
        <h2>Resources</h2>
        <div id="docdls">
          <dl>
            <dt><a href="docs/style-guide">Haskell Style Guide</a></dt>
            <dd>A guide to the Haskell source style we're using for the project.</dd>
            <dt><a href="/benchmarks">Benchmarks</a></dt>
            <dd>Some benchmark results comparing Snap to several other
              web frameworks.</dd>
          </dl>
        </div>
      </div>
    </div>

    <div id="about" class="section left">
      <div class="inner">
        <h2>API Documentation</h2>

        <div id="docdls">
          <dl>
            <dt><a href="docs/api/snap-core">snap-core</a></dt>
            <dd>Core type definitions (<tt>Snap</tt> monad, HTTP types, etc) and
              utilities for web handlers.</dd>
            <dt><a href="docs/api/snap-server">snap-server</a></dt>
            <dd>An iteratee-based HTTP server library, which runs <tt>Snap</tt>
              web handlers.</dd>
            <dt><a href="docs/api/heist">heist</a>
              <em>(experimental)</em></dt>
            <dd>An xhtml-based templating engine, allowing Haskell functions to
              be bound to XML tags.</dd>
          </dl>
        </div>
      </div>
    </div>
  </static>
</apply>
