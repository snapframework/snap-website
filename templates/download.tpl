<apply template="page">
    <h2>Downloading Snap</h2>
    <p>The quickest way to get Snap is with Haskell's Cabal.  If you
already have Cabal set up, then getting Snap is just two simple
commands:</p>
    <pre>
      cabal update
      cabal install snap-server
    </pre>

    <p>After Snap is installed, check out the <a href="/quickstart">quick
start</a> for instructions on getting your project started.</p>

    <h2>Snap Packages</h2>
    <p>Snap is made up of three separate packages:</p>
    <table>
      <tr>
        <th>Package</th>
        <th>Hackage</th>
        <th>Source Repo</th>
      </tr>
      <tr>
        <td><dl>
          <dt>snap-core</dt>
          <dd>The core Snap functionality, data structures.</dd>
        </dl></td>
        <td><a
href="http://hackage.haskell.org/package/snap-core">snap-core</a></td>
        <td>git clone http://git.snapframework.com/snap-core.git</td>
      </tr>
      <tr>
        <td><dl>
          <dt>snap-server</dt>
          <dd>The Snap web server with a fast iteratee implementation.</dd>
        </dl></td>
        <td><a
href="http://hackage.haskell.org/package/snap-server">snap-server</a></td>
        <td>git clone http://git.snapframework.com/snap-server.git</td>
      </tr>
      <tr>
        <td><dl>
          <dt>heist</dt>
          <dd>An xml-based templating engine.</dd>
        </dl></td>
        <td><a
href="http://hackage.haskell.org/package/heist">heist</a></td>
        <td>git clone http://git.snapframework.com/heist.git</td>
      </tr>
    </table>
</apply>
