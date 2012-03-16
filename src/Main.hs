{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.Lens.Template
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           Foreign.C.Types
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.StaticPages
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Text.Templating.Heist
import qualified Text.XHtmlCombinators.Escape as XH

data App = App
    { _heist     :: Snaplet (Heist App)
    , _blog      :: Snaplet StaticPages
    }

makeLenses [''App]

instance HasHeist App where heistLens = subSnaplet heist

epochTime :: IO CTime
epochTime = do
    t <- getPOSIXTime
    return $ fromInteger $ truncate t


description :: Text
description = "The snapframework.com website"


appInit :: SnapletInit App App
appInit = makeSnaplet "snap-website" description Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "templates"
    bs <- nestSnaplet "blog" blog $ staticPagesInit "blogdata"
    addSplices [ ("snap-version", serverVersion)
               , ("feed-autodiscovery-link", liftHeist $ textSplice "")
               ]
    wrapHandlers (\h -> catch500 $ withCompression $
                        h <|> setCache (serveDirectory "static"))
    return $ App hs bs


setCache :: MonadSnap m => m a -> m ()
setCache act = do
    pinfo <- liftM rqPathInfo getRequest
    act
    when ("media" `B.isPrefixOf` pinfo) $ do
       expTime <- liftM (+604800) $ liftIO epochTime
       s       <- liftIO $ formatHttpTime expTime
       modifyResponse $
          setHeader "Cache-Control" "public, max-age=604800" .
          setHeader "Expires" s


catch500 :: MonadSnap m => m a -> m ()
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


serverVersion :: SnapletSplice b v
serverVersion = liftHeist $ textSplice $ T.decodeUtf8 snapServerVersion


main :: IO ()
main = serveSnaplet defaultConfig appInit
