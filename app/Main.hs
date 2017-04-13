module Main where

import Text.Html2Code
import qualified Text.Html2Code.Writers.Hyperscript as Hyperscript
import System.IO

main :: IO ()
main =
    getContents >>= pure . parseTree >>= Hyperscript.write (hPutStrLn stdout) >>= putStr
