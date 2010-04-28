{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (throwIO, SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Reader.Class
import           Control.Monad.Trans
import           Data.Typeable
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           System
import           System.Directory
import           System.Exit
import           System.Process
import           Text.Templating.Heist
import           Text.XML.Expat.Tree


renderTmpl :: MVar (TemplateState Snap)
           -> ByteString
           -> Snap ()
renderTmpl tsMVar n = do
    ts <- liftIO $ readMVar tsMVar
    maybe pass writeBS =<< renderTemplate ts n


templateServe :: MVar (TemplateState Snap)
              -> Snap ()
templateServe tsMVar = 
    ifTop (renderTmpl tsMVar "index") <|>
    path "admin/reload" (reloadTemplates tsMVar) <|>
    (renderTmpl tsMVar . B.pack =<< getSafePath)


reloadTemplates :: MVar (TemplateState Snap)
                -> Snap ()
reloadTemplates tsMVar = do
    liftIO $ modifyMVar_ tsMVar $ const $
           liftM bindMarkdownTag $ loadTemplates "templates"
    
site :: MVar (TemplateState Snap) -> Snap ()
site tsMVar = withCompression $ h1 <|> h2 tsMVar


h1 :: Snap ()
h1 = fileServe "static"

h2 :: MVar (TemplateState Snap) -> Snap ()
h2 m = templateServe m
                                  

bindMarkdownTag :: TemplateState Snap -> TemplateState Snap
bindMarkdownTag = bindSplice "markdown" markdownSplice 


data PandocMissingException = PandocMissingException
   deriving (Typeable)

instance Show PandocMissingException where
    show PandocMissingException =
        "Cannot find the \"pandoc\" executable; is it on your $PATH?"

instance Exception PandocMissingException


data MarkdownException = MarkdownException String
   deriving (Typeable)

instance Show MarkdownException where
    show (MarkdownException e) =
        "Markdown error: pandoc replied:\n\n" ++ e

instance Exception MarkdownException


pandoc :: FilePath -> ByteString -> IO ByteString
pandoc pandocPath s = do
    -- using the crummy string functions for convenience here
    let s' = B.unpack s
    (ex, sout, serr) <- readProcessWithExitCode pandocPath args s'

    when (isFail ex) $ throw $ MarkdownException serr
    return $ B.pack sout

  where
    isFail ExitSuccess = False
    isFail _           = True
    args = [ "-S", "--no-wrap" ]


markdownSplice :: Splice Snap
markdownSplice = do
    pdMD <- liftIO $ findExecutable "pandoc"

    liftIO $ B.putStrLn $ B.concat ["pandoc?", B.pack (show pdMD)]

    when (isNothing pdMD) $ liftIO $ throwIO PandocMissingException

    tree <- ask
    let txt = textContent tree

    liftIO $ B.putStrLn "got text"
    liftIO $ B.putStrLn txt

    markup <- liftIO $ pandoc (fromJust pdMD) txt

    liftIO $ B.putStrLn "got markup"
    liftIO $ B.putStrLn markup

    let ee = parse' heistExpatOptions markup
    case ee of
      (Left e) -> liftIO $ throw $ MarkdownException $
                         "Error parsing markdown output: " ++ show e
      (Right n) -> return [n]


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
    tsMVar <- newMVar $ bindMarkdownTag ts
    (try $ httpServe "*" port "achilles"
             (Just "access.log")
             (Just "error.log")
             (site tsMVar)) :: IO (Either SomeException ())

    putStrLn "exiting"
    return ()

