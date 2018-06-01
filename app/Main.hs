module Main where

import qualified Align           as A
import           Control.Monad
import           Data.Char       (isUpper, toLower)
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Options.Generic

data CliParams = CliParams
    { before      :: Maybe Text <?> "Text to insert before delimiters"
    , after       :: Maybe Text <?> "Text to insert after delimiters"
    , replace     :: Maybe Text <?> "Text to replace delimiters with"
    , stripAfter  :: Bool       <?> "Strip spaces after each block"
    , stripBefore :: Bool       <?> "Strip spaces before each block"
    , strip       :: Bool       <?> "Strip spaces before and after each block"
    , delimiter   :: [Text]     <?> "Text to separate columns"
    } deriving (Generic)

shortener :: String -> Maybe Char
shortener "after"     = Just 'a'
shortener "before"    = Just 'b'
shortener "delimiter" = Just 'd'
shortener "replace"   = Just 'r'
shortener "strip"     = Just 's'
shortener _           = Nothing

nameModifier :: String -> String
nameModifier [] = []
nameModifier (c:cs) | isUpper c = '-' : toLower c : nameModifier cs
                    | otherwise = c : nameModifier cs

modifiers :: Modifiers
modifiers = defaultModifiers
    { shortNameModifier = shortener
    , fieldNameModifier = nameModifier
    }

instance ParseRecord CliParams where
    parseRecord = parseRecordWithModifiers modifiers
deriving instance Show CliParams

main :: IO ()
main = do
    opts <- getRecord "Align"
    let alignOpts = A.AlignOptions
            { prefix      = unHelpful $ before opts
            , suffix      = unHelpful $ after opts
            , separators  = fmap A.Separator . unHelpful $ delimiter opts
            , stripAfter  = unHelpful (strip opts) || unHelpful (stripAfter opts)
            , stripBefore = unHelpful (strip opts) || unHelpful (stripBefore opts)
            }
    textLines <- T.lines <$> T.getContents

    void . traverse T.putStrLn $ A.align alignOpts textLines

