module Main where

import Text.Html2Code
import qualified Text.Html2Code.Writers.Hyperscript as Hyperscript
import qualified Text.Html2Code.Writers.Halogen as Halogen
import System.IO
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    writer <- case args of
        [] -> error "please specify a writer"
        "--halogen":_ -> pure Halogen.write
        "--hyperscript":_ -> pure Hyperscript.write

    getContents >>=
        pure . parseTree >>=
        writer (hPutStrLn stdout) >>=
        putStr
