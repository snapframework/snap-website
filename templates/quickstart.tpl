<apply template="page">
<h2>Snap Quick Start</h2>
<p>First, go to the <a href="/download">download</a> page to find
out how to install Snap.  Next, try running the following sample
app.</p>

<pre>
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Snap.Http.Server
import Snap.Types
site = writeBS "hello world"
main = do
  httpServe "*" 8000 "servername"
    (Just "access.log")
    (Just "error.log")
    site
</pre>

<p>Compile and run this program, then point your web browser to
localhost:8000.</p>

<p>Now, add another import for Snap.Util.FileServe and change the
definition of <i>site</i> to</p>

<pre>site = fileServe "."</pre>

<p>This will serve all all files in the current directory.</p>

</apply>

