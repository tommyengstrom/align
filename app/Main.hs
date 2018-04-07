module Main where

import           Align
import           Control.Monad
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

main :: IO ()
main = do
    textLines <- T.lines <$> T.getContents
    void . traverse T.putStrLn $ align (Separator ",") textLines

