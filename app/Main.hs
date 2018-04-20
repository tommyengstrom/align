module Main where

import qualified Align               as A
import           Control.Applicative
import           Control.Monad
import           Data.Maybe
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Generic

data CliParams w = CliParams
    { delimiter   :: w ::: [Text]     <?> "Delimiter to use"
    , before      :: w ::: Maybe Text <?> "Text to insert before delimiters"
    , after       :: w ::: Maybe Text <?> "Text to insert after delimiters"
    , replace     :: w ::: Maybe Text <?> "Text to replace delimiters with"
    , stripAfter  :: w ::: Maybe Bool <?> "Strip spaces after each block"
    , stripBefore :: w ::: Maybe Bool <?> "Strip spaces before each block"
    , strip       :: w ::: Maybe Bool <?> "Strip spaces before and after each block"
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
    let alignOpts = A.AlignOptions
            { prefix      = before opts
            , suffix      = after opts
            , separators  = A.Separator <$> delimiter opts
            , stripAfter  = fromMaybe False $ strip opts <|> stripAfter opts
            , stripBefore = fromMaybe False $ strip opts <|> stripBefore opts
            }
    textLines <- T.lines <$> T.getContents

    void . traverse T.putStrLn $ A.align alignOpts textLines

