import Text.RSTemplate
import System.FilePath
import System.Environment
import qualified Data.ByteString.Char8 as C

loadEnv fp = do 
  l <- readFile fp >>= return . lines
  return $ map (break (== ':')) l
                

buildFile env fp = do 
  tmpl <- ioEvalFile fp
  tmpl (toContext env)

main = do
  args <- getArgs
  env  <- loadEnv (args !! 0)
  buildFile env (args !! 1) >>= C.putStr