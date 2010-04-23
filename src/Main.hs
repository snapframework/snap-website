module Main where

import           System
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Monad.Trans
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
site tsMVar = withCompression $ templateServe tsMVar <|>
                                fileServe "static"

main :: IO ()
main = do
    args <- getArgs
    port <- case args of
                []     -> error "You must specify a port!" >> exitFailure
                port:_ -> return $ read port

    ts <- loadTemplates "templates"
    tsMVar <- newMVar ts
    httpServe "*" port "achilles"
        (Just "access.log")
        (Just "error.log")
        (site tsMVar)

