{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Monad.Trans
import           System.Exit
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           Text.Templating.Heist

renderTmpl :: MVar (TemplateState IO)
           -> ByteString
           -> Snap ()
renderTmpl tsMVar n = do
    ts <- liftIO $ readMVar tsMVar
    maybe pass writeBS =<< liftIO (renderTemplate ts n)

templateServe :: MVar (TemplateState IO)
              -> Snap ()
templateServe tsMVar = 
    ifTop (renderTmpl tsMVar "index") <|>
    path "admin/reload" (reloadTemplates tsMVar) <|>
    (renderTmpl tsMVar . B.pack =<< getSafePath)

reloadTemplates :: MVar (TemplateState IO)
                -> Snap ()
reloadTemplates tsMVar = do
    liftIO $ modifyMVar_ tsMVar (const $ loadTemplates "templates")
    
site :: MVar (TemplateState IO) -> Snap ()
site tsMVar = withCompression $ h1 <|> h2 tsMVar


h1 :: Snap ()
h1 = fileServe "static"

h2 :: MVar (TemplateState IO) -> Snap ()
h2 m = templateServe m
                                  

-- FIXME: remove
killMe :: ThreadId -> Snap ()
killMe t = liftIO (exitSuccess >> killThread t)

main :: IO ()
main = do
    args <- getArgs
    port <- case args of
                []     -> error "You must specify a port!" >> exitFailure
                port:_ -> return $ read port

    ts <- loadTemplates "templates"
    tsMVar <- newMVar ts
    (try $ httpServe "*" port "achilles"
             (Just "access.log")
             (Just "error.log")
             (site tsMVar)) :: IO (Either SomeException ())

    putStrLn "exiting"
    return ()

