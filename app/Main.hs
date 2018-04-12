module Main where

import           Align
import           Control.Monad
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Options.Generic

data CliParams w = CliParams
    { delimiter :: w ::: [Text]     <?> "Delimiter to use"
    , before    :: w ::: Maybe Text <?> "Test to insert before delimiters"
    , after     :: w ::: Maybe Text <?> "Test to insert after delimiters"
    } deriving (Generic)

modifiers :: Modifiers
modifiers = defaultModifiers
    { shortNameModifier = firstLetter}

instance ParseRecord (CliParams Wrapped) where
    parseRecord = parseRecordWithModifiers modifiers
deriving instance Show (CliParams Unwrapped)

main :: IO ()
main = do
    opts <- unwrapRecord "Align" :: IO (CliParams Unwrapped)
    print opts
    textLines <- T.lines <$> T.getContents
    void . traverse T.putStrLn $ align (fmap Separator $ delimiter opts) textLines

