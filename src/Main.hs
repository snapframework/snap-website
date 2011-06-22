{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Control.Monad.Reader
import           Foreign.C.Types
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.StaticPages
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           System
import           System.Posix.Env
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static
import qualified Text.XHtmlCombinators.Escape as XH
import           Text.XmlHtml hiding (Node)



------------------------------------------------------------------------------
-- snapframework.com site state
------------------------------------------------------------------------------
data SiteState = SiteState {
      _origTs         :: TemplateState Snap
    , _currentTs      :: MVar (TemplateState Snap)
    , _staticTagCache :: StaticTagState
    , _blogState      :: MVar StaticPagesState
}


type Site a = ReaderT SiteState Snap a


epochTime :: IO CTime
epochTime = do
    t <- getPOSIXTime
    return $ fromInteger $ truncate t


initSiteState :: IO SiteState
initSiteState = do
    setLocaleToUTF8

    (origTs,staticState) <- bindStaticTag .
                            bindSplice "snap-version" serverVersion .
                            bindSplice "feed-autodiscovery-link" "" .
                            $ emptyTemplateState "templates"

    ets <- loadTemplates "templates" origTs
    let ts = either error id ets
    either (\s -> putStrLn (loadError s) >> exitFailure) (const $ return ()) ets
    tsMVar <- newMVar $ ts

    bs <- loadStaticPages' ts "blogdata"

    return $ SiteState origTs tsMVar staticState bs


data ReloadException = ReloadException String
  deriving (Show, Typeable)

instance Exception ReloadException


reloadSiteState :: SiteState -> IO ()
reloadSiteState ss = do
    clearStaticTagCache $ _staticTagCache ss
    ts <- loadTemplates "templates" $ _origTs ss
    tt <- either (\msg -> throw $ ReloadException $ loadError msg)
                 (\t -> do
                      modifyMVar_ (_currentTs ss) (const $ return t)
                      return t)
                 ts
    reloadStaticPages' tt $ _blogState ss


------------------------------------------------------------------------------
-- General purpose code.  This code will eventually get moved into Snap once
-- we have a good place to put it.
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
renderTmpl :: MVar (TemplateState Snap)
           -> ByteString
           -> Snap ()
renderTmpl tsMVar n = do
    ts <- liftIO $ readMVar tsMVar
    maybe pass (writeBuilder . fst) =<< renderTemplate ts n


templateServe :: MVar (TemplateState Snap)
              -> Snap ()
templateServe tsMVar = do
    p
    modifyResponse $ setContentType "text/html"

  where
    p = ifTop (renderTmpl tsMVar "index") <|>
        (renderTmpl tsMVar . B.pack =<< getSafePath)


loadError :: String -> String
loadError str = "Error loading templates\n"++str



------------------------------------------------------------------------------
-- handlers
------------------------------------------------------------------------------
site :: SiteState -> Snap ()
site ss =
    catch500 $ withCompression $
        route [ ("docs/api", runReaderT apidoc ss)
              , ("admin/reload", runReaderT reload ss)
              , ("blog/", serveStaticPages (_blogState ss)) ] <|>
        templateServe (_currentTs ss) <|>
        (setCache $ serveDirectory "static")

  where
    setCache act = do
        pinfo <- liftM rqPathInfo getRequest
        act
        when ("media" `B.isPrefixOf` pinfo) $ do
           expTime <- liftM (+604800) $ liftIO epochTime
           s       <- liftIO $ formatHttpTime expTime
           modifyResponse $
              setHeader "Cache-Control" "public, max-age=604800" .
              setHeader "Expires" s

catch500 :: Snap a -> Snap ()
catch500 m = (m >> return ()) `catch` \(e::SomeException) -> do
    let t = T.pack $ show e
    putResponse r
    writeBS "<html><head><title>Internal Server Error</title></head>"
    writeBS "<body><h1>Internal Server Error</h1>"
    writeBS "<p>A web handler threw an exception. Details:</p>"
    writeBS "<pre>\n"
    writeText $ XH.escape t
    writeBS "\n</pre></body></html>"

    logError $ B.concat [ "caught exception: ", B.pack $ show e ]
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse

reload :: Site ()
reload = do
    e <- try (ask >>= liftIO . reloadSiteState)
    lift $ do
        either bad good e
        modifyResponse $ setContentType "text/plain; charset=utf-8"

  where
    bad :: SomeException -> Snap ()
    bad msg = writeBS $ B.pack $ loadError (show msg) ++ "Keeping old templates."
    good _ = writeBS "Templates loaded successfully"


apidoc :: Site ()
apidoc = do
    ss <- ask

    lift $ do
        ts <- liftIO $ readMVar $ _currentTs ss
        -- remainder of pathInfo is the doc to lookup
        whichDoc <- liftM (T.decodeUtf8 . rqPathInfo) getRequest

        title <- maybe pass return $ Map.lookup whichDoc titles
        let href = T.concat ["/docs/latest/", whichDoc, "/index.html"]

        let ts' = bindSplice "docframe" (docframe href) $
                  bindSplice "subtitle" (return [TextNode title]) ts

        modifyResponse $ setContentType "text/html"
        maybe pass (writeBuilder . fst) =<< renderTemplate ts' "docs/api"

  where
    titles = Map.fromList [ ("snap-core", ": snap-core APIs")
                          , ("snap-server", ": snap-server APIs")
                          , ("heist", ": heist APIs") ]

    docframe :: Text -> Splice Snap
    docframe src = return [ Element "frame" [ ("id" , "docframe")
                                            , ("src", src       ) ] [] ]





------------------------------------------------------------------------------
-- MISC UTILITIES
------------------------------------------------------------------------------
serverVersion :: Splice Snap
serverVersion = return [TextNode (T.decodeUtf8 snapServerVersion)]


setLocaleToUTF8 :: IO ()
setLocaleToUTF8 = do
    mapM_ (\k -> setEnv k "en_US.UTF-8" True)
          [ "LANG"
          , "LC_CTYPE"
          , "LC_NUMERIC"
          , "LC_TIME"
          , "LC_COLLATE"
          , "LC_MONETARY"
          , "LC_MESSAGES"
          , "LC_PAPER"
          , "LC_NAME"
          , "LC_ADDRESS"
          , "LC_TELEPHONE"
          , "LC_MEASUREMENT"
          , "LC_IDENTIFICATION"
          , "LC_ALL" ]


------------------------------------------------------------------------------
-- main
------------------------------------------------------------------------------
main :: IO ()
main = do
    ss <- initSiteState

    quickHttpServe (site ss)

    putStrLn "exiting"
    return ()
