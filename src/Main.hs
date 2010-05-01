{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Maybe
import qualified Data.Text as T
import           Control.Applicative
import           Control.Concurrent
import           Control.Exception (throwIO, ErrorCall(..), SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import           Data.Typeable
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.FileServe
import           Snap.Util.GZip
import           System
import           System.Directory
import           System.Exit
import           System.Process
import           Text.Templating.Heist
import qualified Text.XHtmlCombinators.Escape as XH
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
site tsMVar = catch500 $ withCompression $ h1 <|> h2 tsMVar <|> h3


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

  where
    r = setResponseStatus 500 "Internal Server Error" emptyResponse 


h1 :: Snap ()
h1 = fileServe "static"

h2 :: MVar (TemplateState Snap) -> Snap ()
h2 m = templateServe m
                                  
h3 :: Snap ()
h3 = path "throwException" (throw $ ErrorCall "jlkfdjfldskjlf")


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


pandoc :: FilePath -> FilePath -> IO ByteString
pandoc pandocPath inputFile = do
    putStrLn =<< getCurrentDirectory
    (ex, sout, serr) <- readProcessWithExitCode pandocPath args ""

    when (isFail ex) $ throw $ MarkdownException serr
    return $ B.concat [ "<div class=\"markdown\">\n"
                      , UTF8.fromString sout
                      , "\n</div>" ]

  where
    isFail ExitSuccess = False
    isFail _           = True

    -- FIXME: hardcoded path
    args = [ "-S", "--no-wrap", "templates/"++inputFile ]


pandocBS :: FilePath -> ByteString -> IO ByteString
pandocBS pandocPath s = do
    -- using the crummy string functions for convenience here
    let s' = UTF8.toString s
    (ex, sout, serr) <- readProcessWithExitCode pandocPath args s'

    when (isFail ex) $ throw $ MarkdownException serr
    return $ B.concat [ "<div class=\"markdown\">\n"
                      , UTF8.fromString sout
                      , "\n</div>" ]

  where
    isFail ExitSuccess = False
    isFail _           = True
    args = [ "-S", "--no-wrap" ]


markdownSplice :: Splice Snap
markdownSplice = do
    pdMD <- liftIO $ findExecutable "pandoc"

    when (isNothing pdMD) $ liftIO $ throwIO PandocMissingException

    tree <- getParamNode
    markup <- liftIO $
        case getAttribute tree "file" of
            Just f  -> pandoc (fromJust pdMD) $ B.unpack f
            Nothing -> pandocBS (fromJust pdMD) $ textContent tree

    let ee = parse' heistExpatOptions markup
    case ee of
      (Left e) -> throw $ MarkdownException $
                         "Error parsing markdown output: " ++ show e
      (Right n) -> return [n]


-- FIXME: remove
killMe :: ThreadId -> Snap ()
killMe t = liftIO (exitSuccess >> killThread t)

main :: IO ()
main = do
    args   <- getArgs
    port   <- case args of
                []       -> error "You must specify a port!" >> exitFailure
                (port:_) -> return $ read port

    ts     <- loadTemplates "templates"
    tsMVar <- newMVar $ bindMarkdownTag ts

    (try $ httpServe "*" port "achilles"
             (Just "access.log")
             (Just "error.log")
             (site tsMVar)) :: IO (Either SomeException ())

    putStrLn "exiting"
    return ()

