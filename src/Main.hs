module Main where

import Directory
import System

import Control.Applicative
import Control.Monad.Trans
import Snap.Http.Server
import Snap.Types
import Snap.Util.FileServe
import System.FilePath
import Text.Templating.Heist

--templateServe :: TemplateState IO
--              -> Snap ()
--templateServe ts = do
--    req <- getRequest
--    let pInfo = S.unpack $ rqPathInfo req
--    fp <- resolvePath pInfo
--    let fn   = takeFileName fp
--  where
--    resolvePath p = do
--        -- relative paths only!
--        when (not $ isRelative p) pass
--
--        -- check that we don't have any sneaky .. paths
--        let dirs = splitDirectories p
--        when (elem ".." dirs) pass
--
--        let f = root </> p
--
--        -- check that the file exists
--        liftIO (doesFileExist f) >>= flip unless pass
--
--        return f

site :: TemplateState IO -> Snap ()
site ts = 
    ifTop (maybe pass writeBS =<< liftIO (renderTemplate ts "index")) <|>
    template "about" <|>
    template "download" <|>
    template "docs" <|>
    template "contribute" <|>
    template "news" <|>
    template "heist-tutorial" <|>
    template "style-guide" <|>
    template "tutorials/heist" <|>
    fileServe "static"
  where
    template n = path n (maybe pass writeBS =<< liftIO (renderTemplate' "templates" n))

main :: IO ()
main = do
    args <- getArgs
    port <- case args of
                []     -> error "You must specify a port!" >> exitFailure
                port:_ -> return $ read port

    templateState <- loadTemplates "templates"
    httpServe "*" port "achilles"
        (Just "access.log")
        (Just "error.log")
        (site templateState)

