module Main where

import           Align
import           Control.Monad
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Options.Generic

data CliParams w = CliParams
    { delimiter :: w ::: [Text]     <?> "Delimiter to use"
    , before    :: w ::: Maybe Text <?> "Text to insert before delimiters"
    , after     :: w ::: Maybe Text <?> "Text to insert after delimiters"
    , replace   :: w ::: Maybe Text <?> "Text to replace delimiters with"
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
    let alignOpts = AlignOptions
            { prefix     = before opts
            , suffix     = after opts
            , separators = Separator <$> delimiter opts
            }
    textLines <- T.lines <$> T.getContents

    void . traverse T.putStrLn $ alignApa alignOpts textLines

