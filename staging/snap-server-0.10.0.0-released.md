| title: Announcing: Snap Server v0.10.0.0
| author: Robert Massaioli <robertmassaioli@gmail.com>
| published: 2015-06-18T20:15:00-0800
| updated:   2015-06-18T20:15:00-0800
| summary: Release notes for Snap Server v0.10.0.0

The Snap team is happy to announce the release of snap-server 0.10.

## Changes

This release of snap-server contains one major change from 0.9.5.0: the
addition of custom logging.

### Custom Access and Error logging for Snap Server

In this release of snap-server we include two new configuration options aimed
at letting you control the logging format of snap-server.

Before version 0.10, snap-server wrote its access log in a fixed format (the
standard
[NCSA Common Log Format](https://en.wikipedia.org/wiki/Common_Log_Format). For
example, it should be familiar to see a log line like the following from your
snap application:

    127.0.0.1 - - [10/Jun/2015:14:12:55 +1000] "POST /rest/round-rooms HTTP/1.1" 200 - "http://localhost:8080/panel/hackathon-round-transition?project_id=10000&project_key=SP" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36"

And the error log is also written in a fixed ad-hoc format that looks like this:

    [04/Dec/2014:11:11:57 +1100] Server.httpServe: START, binding to [http://0.0.0.0:8080/]

Version 0.10 of snap-server adds configuration hooks that allow you to override
these choices and log in a custom format.

### Custom Access Logging (a working example)

To create your own custom access and error logging formats, snap-server
provides two new configuration options: `setAccessLogHandler` and
`setErrorLogHandler`. These methods will let you control how the access and
error log lines are rendered. Let's run through a quick example with the
setAccessLogHandler method.

```haskell
type AccessLogHandler = Request -> Response -> IO ByteString
setAccessLogHandler :: AccessLogHandler -> Config m a -> Config m a
```

As you can see, the `AccessLogHandler` is passed the `Request` and final
user-generated `Response`, and using the information inside you can log
whatever you'd like. Let's now show a working example of actually using these
new types to log in JSON format using the
[Aeson](http://hackage.haskell.org/package/aeson) library. First, let's define
a datatype that will represent the JSON data that should appear on each line of
our log file:

```haskell
-- import qualified Data.Text as T
data AccessLogLine = AccessLogLine
    { allEvent                 :: String
    , allTimestamp             :: UTCTime
    , allHost                  :: T.Text
    , allMethod                :: T.Text
    , allUrl                   :: T.Text
    , allHttpVersion           :: T.Text
    , allStatus                :: Int
    , allResponseContentLength :: Maybe Int64
    , allReferer               :: Maybe T.Text
    , allUserAgent             :: Maybe T.Text
    } deriving (Show, Generic)
```

As you can see, this definition contains all of the information we plan on
taking from the request and response. We can now define a `ToJSON` instance for
this data type so that we can encode it easily. We will use aeson's nifty
Generic support and a few helper methods to define the instance:

```haskell
instance ToJSON AccessLogLine where
   toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = stripFieldNamePrefix "all"
        }

stripFieldNamePrefix :: String -> String -> String
stripFieldNamePrefix pre = lowerFirst . try (L.stripPrefix pre)    

lowerFirst :: String -> String
lowerFirst (x : xs) = C.toLower x : xs
lowerFirst [] = []
```

Now all that remains is to define the AccessLogHandler itself, which will take
the request and response, put the relevant data into an AccessLogLine, and
return the encoded version of the AccessLogLine in JSON format:

```haskell
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Snap.Core                  as SC
import qualified Snap.Types.Headers         as H
accessLogHandler :: AccessLogHandler
accessLogHandler req rsp = do
    let hdrs      = SC.headers req
    let host      = SC.rqRemoteAddr req
    let (v, v')   = SC.rqVersion req
    let referer   = head <$> H.lookup "referer" hdrs
    let userAgent = head <$> H.lookup "user-agent" hdrs
    currentTime <- getCurrentTime
    return . L.toStrict . encode $! AccessLogLine
        { allEvent          = "log.access"
        , allTimestamp      = currentTime
        , allHost           = decU8 host
        , allMethod         = tshow . SC.rqMethod $ req
        , allUrl            = decU8 . SC.rqURI $ req
        , allHttpVersion    = T.concat [ tshow v, ".", tshow v' ]
        , allStatus         = SC.rspStatus rsp
        , allResponseContentLength = rspContentLength rsp
        , allReferer        = decU8 <$> referer
        , allUserAgent      = decU8 <$> userAgent
        }
    where
        tshow :: Show a => a -> T.Text
        tshow = T.pack . show
        decU8 = T.decodeUtf8
```

Once this function is plugged into the snap-server `Config` datatype using
`setAccessLogHandler`, the snap-server will start outputting its access logs in
JSON format. Note also that since `AccessLogHandler` lives in `IO`, you can
perform arbitrary side-effects here, so for example you could send your access
log message directly to a syslog.

We hope that this change allows for better integration with third party tools
or simply allows you to extract the information that you need from snap-server.
Good luck and keep writing awesome web apps in snap!
