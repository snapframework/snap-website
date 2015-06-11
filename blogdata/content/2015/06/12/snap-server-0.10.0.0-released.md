| title: Announcing: Snap Server v0.10.0.0
| author: Robert Massaioli <robertmassaioli@gmail.com>
| published: 2015-06-12T06:10:00+1000
| updated:   2015-06-12T06:10:00+1000
| summary: Release notes for Snap Server v0.10.0.0

The snap team is happy to announce the release of version 0.10.0.0 of snap-server.

## Changes

This release of snap-server only contains one major change from 0.9.5.0 and that is the addition of
custom logging.

### Custom Access and Error logging for Snap Server

In this release of snap-server we include two new configuration options aimed at letting you control
the logging format of snap-server.

Currently snap-server access logs appear in a format that resembles a common Apache httpd log format. For example,
it should be familiar to see a log line like the following from your snap application:

    127.0.0.1 - - [10/Jun/2015:14:12:55 +1000] "POST /rest/round-rooms HTTP/1.1" 200 - "http://localhost:8080/panel/hackathon-round-transition?project_id=10000&project_key=SP" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/43.0.2357.81 Safari/537.36"

And the error log is logged in a similar, but not quite the same, format and looks like:

    [04/Dec/2014:11:11:57 +1100] Server.httpServe: START, binding to [http://0.0.0.0:8080/]

With the latest release of snap-server you are now free to define your own custom logging format! If
you choose to not define you own custom format then the access and error logs will continue to
appear in the same format.

### Custom Access Logging (a working example)

To create you own custom access and error logging formats snap-server provides two new configuration 
options setAccessLogHandler and setErrorLogHandler respectively. These methods will let you control 
how the access and error log lines are rendered. Lets run through a quick example with the
setAccessLogHandler method. But first lets look at the types! The first type we will look at is:

    setAccessLogHandler :: AccessLogHandler      -> Config m a -> Config m a

This method accepts an AccessLogHandler and updates your snap-server configuration to use it. But
what is an AccessLogHandler? An AccessLogHandler lets you get contextual information from the
current request / response and output access log lines as a result and thus is defined as the
following simple type alias:

    type AccessLogHandler = Request -> Response -> IO ByteString

As you can see the AccessLogHandler is passed the Snap Request and Response so that you can log
whatever you like in a context sensitive manner. Lets now show a working example of actually using
these new types to log in JSON format using the [Aeson][1] library. First lets define a datatype that
will represent the JSON data that should appear on each line of our log file:

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

As you can see this definition contains a lot of information about the request and response. We can
now define a ToJSON instance for this data type so that we can encode it easily. We will use the
derived Generic instance and a few helper methods to define the ToJSON instance:
    
    instance ToJSON AccessLogLine where
       toJSON = genericToJSON defaultOptions
            { fieldLabelModifier = stripFieldNamePrefix "all"
            }

    stripFieldNamePrefix :: String -> String -> String
    stripFieldNamePrefix pre = lowerFirst . try (L.stripPrefix pre)    

    lowerFirst :: String -> String
    lowerFirst (x : xs) = C.toLower x : xs
    lowerFirst [] = []

Now it is a simple matter of defining the AccessLogHandler itself which will take the request and
response, put the relevant data into an AccessLogLine and then return the encoded version of the
AccessLogLine in JSON format:

    -- import qualified Data.Text                  as T
    -- import qualified Data.Text.Encoding         as T
    -- import qualified Snap.Core                  as SC
    -- import qualified Snap.Types.Headers         as H
    accessLogHandler :: AccessLogHandler
    accessLogHandler req rsp = do
        let hdrs      = SC.headers req
        let host      = SC.rqRemoteAddr req
        let (v, v')   = SC.rqVersion req
        let referer   = head <$> H.lookup "referer" hdrs
        let userAgent = head <$> H.lookup "user-agent" hdrs
    
        currentTime <- getCurrentTime
        return . L.toStrict . encode $ AccessLogLine
            { allEvent          = "log.access"
            , allTimestamp      = currentTime
            , allHost           = t host
            , allMethod         = tshow . SC.rqMethod $ req
            , allUrl            = t . SC.rqURI $ req
            , allHttpVersion    = T.concat [ tshow v, ".", tshow v' ]
            , allStatus         = SC.rspStatus rsp
            , allResponseContentLength = rspContentLength rsp
            , allReferer        = t <$> referer
            , allUserAgent      = t <$> userAgent
            }
        where
            tshow :: Show a => a -> T.Text
            tshow = T.pack . show
            t = T.decodeUtf8

As you can see there is nothing overly complicated going on inside our accessLogHandler but now we
can use the following code to update our snap-server configuration:

    setAccessLogHandler accessLogHandler 
    
And now the access logs for our snap application running on snap-server are being output in JSON
format! You are now free to reuse this code in your own applications or to define your own custom
formats. Please let us know what formats you were able to define that were the most useful to you.

We hope that this change allows for better integration with third party tools or simply allows you to
extract the information that you need from snap-server. Good luck and keep writing awesome web
apps in snap!

 [1]: http://hackage.haskell.org/package/aeson
