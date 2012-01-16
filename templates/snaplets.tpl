<bind tag="subtitle">: Snaplet Directory</bind>
<apply template="page">
  <static>
    <div id="about" class="section left">
      <div class="inner">
        <h2>About Snaplets</h2>
        <p><img src="/media/img/snaplet.png" align="left" style="display: inline; padding: 0 14px 14px 14px;" /><strong><a href="docs/tutorials/snaplets-tutorial">Snaplets</a></strong> are self
        contained pieces of functionality that you can include in your web
        apps.</p>  <p>You can develop your own snaplets and publish them as a
        standalone project on hackage completely independant of the Snap team.
        For convenience we're going to keep this page updated with all the
        snaplets we know about.
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
          <p>These snaplets are not maintained by the Snap Framework team.  If
          you know of a snaplet not listed here, please <a
          href="mailto:snap@snapframework.com">let us know</a>.
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

            <dt>snaplet-mongoDB
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-mongoDB"
                   > hackage </a>
                  | <a href="https://bitbucket.org/kamilc/snaplet-mongodb"
                   >bitbucket</a> ]
                </span>
            </dt>
            <dd>MongoDB support</dd>

            <dt>snaplet-mongodb-minimalistic
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-mongodb-minimalistic"
                   > hackage </a>
                  | <a href="https://github.com/Palmik/snaplet-mongodb-minimalistic"
                   >github</a> ]
                </span>
            </dt>
            <dd>MongoDB support</dd>

            <dt>snaplet-sedna
                <span class="linklist">
                  [ <a href="http://hackage.haskell.org/package/snaplet-sedna"
                   > hackage </a>
                  | <a href="https://github.com/ExternalReality/snaplet-sedna"
                   >github</a> ]
                </span>
            </dt>
            <dd>Support for the Sedna XML database.</dd>

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

          </dl>
        </div>
      </div>
    </div>
  </static>
</apply>
