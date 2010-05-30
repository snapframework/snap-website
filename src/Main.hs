{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as T
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           System
import           System.Posix.Env
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static
import qualified Text.XHtmlCombinators.Escape as XH
import           Text.XML.Expat.Tree hiding (Node)


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
    maybe pass writeBS =<< renderTemplate ts n


templateServe :: TemplateState Snap
              -> MVar (TemplateState Snap)
              -> StaticTagState
              -> Snap ()
templateServe orig tsMVar staticState = do
    p
    modifyResponse $ setContentType "text/html"

  where
    p = ifTop (renderTmpl tsMVar "index") <|>
        path "admin/reload" (reloadTemplates orig tsMVar staticState) <|>
        (renderTmpl tsMVar . B.pack =<< getSafePath)


loadError :: String -> String
loadError str = "Error loading templates\n"++str

reloadTemplates :: TemplateState Snap
                -> MVar (TemplateState Snap)
                -> StaticTagState
                -> Snap ()
reloadTemplates origTs tsMVar staticState = do
    liftIO $ clearStaticTagCache staticState
    ts <- liftIO $ loadTemplates "templates" origTs
    either bad good ts
  where
    bad msg = do writeBS $ B.pack $ loadError msg ++ "Keeping old templates."
    good ts = do liftIO $ modifyMVar_ tsMVar (const $ return ts)
                 writeBS "Templates loaded successfully"


site :: TemplateState Snap
     -> MVar (TemplateState Snap)
     -> StaticTagState
     -> Snap ()
site origTs tsMVar staticState =
    catch500 $ withCompression $
        route [ ("docs/api", apidoc tsMVar) ] <|>
        templateServe origTs tsMVar staticState <|>
        fileServe "static"


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


apidoc :: MVar (TemplateState Snap) -> Snap ()
apidoc mvar = do
    ts <- liftIO $ readMVar mvar
    -- remainder of pathInfo is the doc to lookup
    whichDoc <- liftM rqPathInfo getRequest

    title <- maybe pass return $ Map.lookup whichDoc titles
    let href = B.concat ["/docs/latest/", whichDoc, "/index.html"]

    let ts' = bindSplice "docframe" (docframe href) $
              bindSplice "subtitle" (return [mkText title]) ts

    modifyResponse $ setContentType "text/html"
    maybe pass writeBS =<< renderTemplate ts' "docs/api"

  where
    titles = Map.fromList [ ("snap-core", ": snap-core APIs")
                          , ("snap-server", ": snap-server APIs")
                          , ("heist", ": heist APIs") ]

    docframe :: ByteString -> Splice Snap
    docframe src = return [ mkElement "frame" [ ("id" , "docframe")
                                              , ("src", src       ) ] [] ]

serverVersion :: Splice Snap
serverVersion = return $ [Text (B.append "Snap-" snapServerVersion)]


main :: IO ()
main = do
    args   <- getArgs
    port   <- case args of
                []       -> error "You must specify a port!" >> exitFailure
                (port:_) -> return $ read port

    setLocaleToUTF8

    (origTs,staticState) <- bindStaticTag .
                            bindSplice "server" serverVersion
                            $ emptyTemplateState

    ets <- loadTemplates "templates" origTs
    let ts = either error id ets
    either (\s -> putStrLn (loadError s) >> exitFailure) (const $ return ()) ets
    tsMVar <- newMVar $ ts

    (try $ httpServe "*" port "myserver"
             (Just "access.log")
             (Just "error.log")
             (site origTs tsMVar staticState)) :: IO (Either SomeException ())

    threadDelay 1000000
    putStrLn "exiting"
    return ()

