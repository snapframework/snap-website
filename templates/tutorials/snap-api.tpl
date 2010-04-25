<apply template="page">
<div id="getting-started-with-snap"
><h2
  >Getting Started with Snap</h2
  ><p
  >This document provides an overview of some of the Snap API functions that you will use most frequently in web development.</p
  ></div
><div id="creating-responses"
><h2
  >Creating Responses</h2
  ><p
  >Snap provides direct support for creating responses from ByteStrings and Enumerators. Snap.Types has three functions for this:</p
  ><ul
  ><li
    >writeBS :: ByteString -&gt; Snap ()</li
    ><li
    >writeLBS :: ByteString -&gt; Snap ()</li
    ><li
    >addToOutput :: Enumerator a -&gt; Snap ()</li
    ></ul
  ><p
  >writeBS and writeLBS are for writing strict and lazy ByteStrings respectively. addToOutput is for writing data from an Enumerator.</p
  ></div
><div id="routing"
><h2
  >Routing</h2
  ><p
  >The Haddock documentation for Snap has a <a href="http://mightybyte.net:8000/docs/snap-core/Snap-Types.html#8"
    >section on routing functions</a
    > that covers this material in detail, this will just give a brief overview. There are 5 core functions for routing:</p
  ><ul
  ><li
    >method</li
    ><li
    >path</li
    ><li
    >dir</li
    ><li
    >ifTop</li
    ><li
    >route</li
    ></ul
  ><p
  >If you have used Happstack, these functions should sound familiar. The method function lets you match on specific HTTP method types, GET, POST, PUT, etc. The path function lets you match a complete path exactly. The dir function matches on the first &quot;directory&quot; component of the path. ifTop filters for the requests to the root of the site (i.e. http://mysite.com/).</p
  ><p
  >The route function takes a list of (path, handler) tuples and provides efficient routing to the correct handler. It also provides a mechanism for capturing path components to variables. The captured variables are stored in the Request parameters.  You should use the route function for as much of your site as possible because it matches routes in O(log n) time as opposed to O(n) without it.  The other routing functions should only be used for things the route function can't handle like matching on the method.</p
  ></div
><div id="request-parameters"
><h2
  >Request Parameters</h2
  ><p
  >Snap has five main functions for working with Request parameters. Parameters are automatically parsed from the query string and POST body.</p
  ><ul
  ><li
    >rqParams</li
    ><li
    >rqParam</li
    ><li
    >getParam</li
    ><li
    >rqModifyParams</li
    ><li
    >rqSetParam</li
    ></ul
  ><p
  >These are fairly self-explanatory. rqParams gets all the parameters. rqParam and getParam get a single parameter. If the parameter was specified multiple times, rqParam will return a list of values, but getParam will concatenate the values together separated by spaces. rqModifyParams provides a way to generically modify the parameters within your application, and rqSetParam provides a convenient way to set a single parameter.</p
  ></div
><div id="flow-control"
><h2
  >Flow Control</h2
  ><p
  >The finishWith and pass functions give you control over the flow of Request processing. finishWith stops all processing and immediately returns the supplied Response. pass stops processing but proceeds to the next Alternative.</p
  ></div
><div id="typeclass-instances"
><h2
  >Typeclass Instances</h2
  ><p
  >The Snap monad provides type class instances for MonadPlus and Alternative. For those unfamiliar with these Haskell type classes, a short description is instructive. MonadPlus gives you the two functions mzero and mplus. The laws satisfied by mzero, mplus, and bind (the &gt;&gt;= operator) are instructive in understanding them.</p
  ><pre
  >
  mzero &gt;&gt;= f = mzero
  mzero `mplus` v = v
  v `mplus` mzero = v</pre
  ><p
  >An mzero as the first value of a bind short-circuits the flow and prevents the second value from being executed. But with mplus it is the opposite--a <em
    >non-mzero</em
    > as the first value prevents the second value from being executed. Let me restate that another way. mzero represents failure. A string of binds continues as long as the computations succeed, but stops on the first failure. A string of mpluses stops on the first success.</p
  ><p
  >The Snap instance of Alternative just provides synonyms for the MonadPlus behavior with &quot;empty = mzero&quot; and &quot;(&lt;|&gt;) = mplus&quot;. The pass function mentioned above is just a synonym for mzero.</p
  ><p
  >For more in-depth discussion of these and other commond Haskell type classes, see Brent Yorgey's outstanding <a href="http://haskell.org/sitewiki/images/8/85/TMR-Issue13.pdf"
    >Typeclassopedia</a
    >.</p
  ></div
>
</apply>
