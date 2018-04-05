module Main where

import Align
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Monad

main :: IO ()
main = do
    textLines <- T.lines <$> T.getContents
    void . traverse T.putStrLn $ align (Separator ",") textLines