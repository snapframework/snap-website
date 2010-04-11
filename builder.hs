import Text.RSTemplate
import System.FilePath
import System.Environment
import qualified Data.ByteString.Char8 as C

env = [("main-fg","#2c558a")
      ,("fonts","\"Helvetica Neue\", \"Arial\", \"Helvetica\", sans-serif;")]

------------------------------------------------------------------------

buildFile fp = do tmpl <- ioEvalFile fp
                  tmpl (toContext env)

main = do
  args <- getArgs
  buildFile (head args) >>= C.putStr