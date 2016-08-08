<bind tag="subtitle">: Snaplet Directory</bind>
<apply template="page">
  <static>
    <div id="about" class="section left">
      <div class="inner">
        <h2>About Snaplets</h2>
        <p>

          <img src="/media/img/snaplet.png" align="left" style="display: inline;
          padding: 0 14px 14px 14px;" /><strong><a
          href="docs/tutorials/snaplets-tutorial">Snaplets</a></strong> are self
          contained pieces of composable web functionality that are designed to
          be reused across multiple web apps.

        </p>

        <p>

          Snaplets can define their own routes, include filesystem resources
          such as templates and out of the box configuration, and even depend on
          other snaplets. You can develop your own snaplets and publish them as
          a standalone project on hackage completely independent of the Snap
          team. We highlight a few prominent snaplets here, but a more complete
          list can be found in the <a
          href="http://hackage.haskell.org/packages/#cat:Snap">Snap category on
          hackage</a>.

        </p>
        <h2>Snaplets that come with Snap</h2>
        <div id="snapletdls">
          <dl>
            <dt>heist
                <span class="linklist">
                [ <a href="https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Heist.hs"
                   >github</a> ]</span>
            </dt>
            <dd>Heist templating snaplet</dd>

            <dt>sessions
                <span class="linklist">
                [ <a href="https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Session.hs"
                   >github</a> ]</span>
            </dt>
            <dd>Sessions snaplet with a built-in cookie-based back end</dd>

            <dt>auth
                <span class="linklist">
                [ <a href="https://github.com/snapframework/snap/blob/master/src/Snap/Snaplet/Auth.hs"
                   >github</a> ]</span>
            </dt>
            <dd>Authentication</dd>

          </dl>
        </div>
      </div>
    </div>
    <div id="about" class="section left">
      <div class="inner">
        <h2>Third-Party Snaplets</h2>
        <div id="snapletdls">
          <p>Here is a somewhat arbitrarily selected list of snaplets that are
          not maintained by the Snap Framework team.
          </p>
          <dl>
            <dt>snaplet-acid-state
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-acid-state"
                   > hackage </a>
                  | <a href="https://github.com/mightybyte/snaplet-acid-state"
                   >github</a> ]
                </span>
            </dt>
            <dd>Persist native Haskell data structures with acid-state.</dd>

            <dt>snaplet-file-dialog
                <span class="linklist">
                  [ <a href="http://hub.darcs.net/wlangstroth/snaplet-file-dialog"
                   >darcsden</a> ]
                </span>
            </dt>
            <dd>Upload, download, browse, and delete files on a web server in
            a private file transfer area.</dd>

            <dt>snaplet-hdbc
                <span class="linklist">
                  [ <a href="http://norm2782.github.com/snaplet-hdbc.html"
                   >tutorial</a>
                  | <a href="http://hackage.haskell.org/package/snaplet-hdbc"
                   > hackage </a>
                  | <a href="https://github.com/norm2782/snaplet-hdbc"
                   >github</a> ]
                </span>
            </dt>
            <dd>HDBC database support</dd>

            <dt>snaplet-i18n
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-i18n"
                   > hackage </a>
                  | <a href="https://github.com/HaskellCNOrg/snaplet-i18n"
                   >github</a> ]
                </span>
            </dt>
            <dd>A light weight i18n snaplet</dd>

            <dt>snaplet-liftajax
                <span class="linklist">
                  [ <a href="https://github.com/davidsd/snaplet-liftajax"
                   >github</a> ]
                </span>
            </dt>
            <dd>Provides splices for AJAX-enabled forms and buttons</dd>

            <dt>snaplet-mysql-simple
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-mysql-simple"
                   > hackage </a>
                  | <a href="https://github.com/ibotty/snaplet-mysql-simple"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the MySQL database using the
            <a href="http://hackage.haskell.org/package/mysql-simple">mysql-simple</a>
            library</dd>

            <dt>snaplet-postgresql-simple
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-postgresql-simple"
                   > hackage </a>
                  | <a href="https://github.com/mightybyte/snaplet-postgresql-simple/"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the PostgreSQL database using the
            <a href="http://hackage.haskell.org/package/postgresql-simple">postgresql-simple</a>
            library</dd>

            <dt>snaplet-postmark
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-postmark"
                   > hackage </a>
                  | <a href="https://github.com/LukeHoersten/snaplet-postmark"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the <a href="http://postmarkapp.com">Postmark email system</a>.</dd>

            <dt>snaplet-purescript
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-purescript"
                   > hackage </a>
                  | <a href="https://github.com/adinapoli/snaplet-purescript"
                   >github</a> ]
                </span>
            </dt>
            <dd>Automatic (re)compilation of purescript projects.</dd>

            <dt>snaplet-recaptcha
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-recaptcha"
                   > hackage </a>
                  | <a href="https://github.com/lpeterse/snaplet-recaptcha/"
                   >github</a> ]
                </span>
            </dt>
            <dd>A ReCAPTCHA verification snaplet with connection sharing.</dd>

            <dt>snaplet-redis
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-redis"
                   > hackage </a>
                  | <a href="https://github.com/dzhus/snaplet-redis/"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the redis in-memory key-value store.</dd>

            <dt>snaplet-redson
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-redson"
                   > hackage </a>
                  | <a href="https://github.com/dzhus/snaplet-redson/"
                   >github</a> ]
                </span>
            </dt>
            <dd>CRUD for JSON data with Redis storage.</dd>

            <dt>snaplet-sedna
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-sedna"
                   > hackage </a>
                  | <a href="https://github.com/ExternalReality/snaplet-sedna"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the Sedna XML database.</dd>

            <dt>snaplet-sqlite-simple
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-sqlite-simple"
                   > hackage </a>
                  | <a href="https://github.com/nurpax/snaplet-sqlite-simple"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for sqlite.</dd>
            
            <dt>snaplet-stripe
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-stripe"
                   > hackage </a>
                  | <a href="https://github.com/LukeHoersten/snaplet-stripe"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for <a href="http://stripe.com">Stripe payments</a>.</dd>

            <dt>snaplet-tasks
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-tasks"
                   > hackage </a>
                  | <a href="https://bitbucket.org/kamilc/snaplet-tasks"
                   >bitbucket</a> ]
                </span>
            </dt>
            <dd>Allows the creation of command line tasks akin to "rake tasks"
            from Ruby on Rails.</dd>

            <dt>snaplet-typed-sessions
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-typed-sessions"
                   > hackage </a> ]
                </span>
            </dt>
            <dd>Typed session snaplets and continuation-based programming</dd>

          </dl>
        </div>
      </div>
    </div>
  </static>
</apply>
