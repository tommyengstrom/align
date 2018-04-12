module Main where

import           Align
import           Control.Monad
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import Options.Generic

data CliParams w = CliParams
    { delimiter :: w ::: [Text] <?> "Delimiter to use"
    } deriving (Generic)

instance ParseRecord (CliParams Wrapped)
deriving instance Show (CliParams Unwrapped)

main :: IO ()
main = do
    opts <- unwrapRecord "Align" :: IO (CliParams Unwrapped)
    print opts
    textLines <- T.lines <$> T.getContents
    void . traverse T.putStrLn $ align (fmap Separator $ delimiter opts) textLines

