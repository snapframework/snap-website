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
import qualified Data.Text as T
import           Data.Text.Encoding
import           Data.Time.Clock.POSIX
import           Foreign.C.Types
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.StaticPages
import           Snap.Blaze (blaze)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Text.Blaze.Html5 (toHtml)
import qualified Text.Blaze.Html5 as H
import           Text.Templating.Heist

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


undirify :: MonadSnap m => m ()
undirify = do
    uri <- withRequest (return . rqURI)
    if B.length uri > 1 && T.last (decodeUtf8 uri) == '/'
      then redirect (B.init uri)
      else return ()


appInit :: SnapletInit App App
appInit = makeSnaplet "snap-website" description Nothing $ do
    hs <- nestSnaplet "" heist $ heistInit "templates"
    bs <- nestSnaplet "blog" blog $ staticPagesInit "blogdata"
    addSplices [ ("snap-version", serverVersion)
               , ("feed-autodiscovery-link", liftHeist $ textSplice "")
               ]
    wrapHandlers (\h -> catch500 $ withCompression $
                        undirify >> h <|> setCache (serveDirectory "static"))
    return $ App hs bs
  where
    description = "The snapframework.com website"


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
    blaze $ do
      H.docType
      H.html $ do
        H.head $ H.title "Internal Server Error"
        H.body $ do
          H.h1 "Internal Server Error"
          H.p "A web handler threw an exception. Details:"
          H.pre $ "\n" >> (toHtml t) >> "\n"

    logError $ B.concat [ "caught exception: ", B.pack $ show e ]
  where
    r = setContentType "text/html" $
        setResponseStatus 500 "Internal Server Error" emptyResponse


serverVersion :: SnapletSplice b v
serverVersion = liftHeist $ textSplice $ decodeUtf8 snapServerVersion


main :: IO ()
main = serveSnaplet defaultConfig appInit
