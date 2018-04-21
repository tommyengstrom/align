module Main where

import qualified Align               as A
import           Control.Applicative
import           Control.Monad
import qualified Data.List.NonEmpty  as NE
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Generic

data CliParams = CliParams
    { before      :: Maybe Text <?> "Text to insert before delimiters"
    , after       :: Maybe Text <?> "Text to insert after delimiters"
    , replace     :: Maybe Text <?> "Text to replace delimiters with"
    , stripAfter  :: Maybe Bool <?> "Strip spaces after each block"
    , stripBefore :: Maybe Bool <?> "Strip spaces before each block"
    , strip       :: Maybe Bool <?> "Strip spaces before and after each block"
    } deriving (Generic)

modifiers :: Modifiers
modifiers = defaultModifiers
    { shortNameModifier = firstLetter}

instance ParseRecord CliParams where
    parseRecord = parseRecordWithModifiers modifiers
deriving instance Show CliParams


data Mixed = Mixed (NE.NonEmpty (Text <?> "Separator")) CliParams
    deriving Generic

instance ParseRecord Mixed where
    parseRecord = Mixed <$> parseRecord <*> parseRecord

main :: IO ()
main = do
    --Mixed seps opts <- (unwrapRecord "Align" :: IO (Mixed Unwrapped))
    Mixed seps opts <- getRecord "Align"
    let alignOpts = A.AlignOptions
            { prefix      = unHelpful $ before opts
            , suffix      = unHelpful $ after opts
            , separators  = A.Separator . unHelpful <$> NE.toList seps
            , stripAfter  = fromMaybe False $ unHelpful (strip opts) <|> unHelpful (stripAfter opts)
            , stripBefore = fromMaybe False $ unHelpful (strip opts) <|> unHelpful (stripBefore opts)
            }
    textLines <- T.lines <$> T.getContents

    void . traverse T.putStrLn $ A.align alignOpts textLines

