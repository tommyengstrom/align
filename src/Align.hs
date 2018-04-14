{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import           Control.Applicative
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Safe
import           Text.Megaparsec
import           Text.Megaparsec.Char

newtype Separator = Separator {unSeparator :: Text}
    deriving (Show, IsString)

newtype Offset = Offset {unOffset :: Int}
    deriving (Show, Eq, Ord, Num)

data Piece = TextBlock Text
           | Delim Text
           deriving (Show, Eq, Ord)

data AlignOptions = AlignOptions
    { prefix     :: Maybe Text
    , suffix     :: Maybe Text
    , separators :: [Separator]
    } deriving Show

defaultOptions :: [Separator] -> AlignOptions
defaultOptions = AlignOptions Nothing Nothing

lineParser :: [Separator] -> Parsec Void Text [Piece]
lineParser ss = do
    parts <- many
           . foldl1 (<|>)
           $ fmap (try . delimP) ss <> [try $ textChunk ss]
    lastPart <- afterLast
    pure $ parts <> [lastPart, Delim ""]

delimP :: Separator -> Parsec Void Text Piece
delimP = fmap Delim . string' . unSeparator

textChunk :: [Separator] -> Parsec Void Text Piece
textChunk ss = fmap (TextBlock . T.pack) $  manyTill anyChar findDelim
    where
        findDelim = lookAhead . foldl1 (<|>) $ fmap delimP ss <> []

afterLast :: Parsec Void Text Piece
afterLast = TextBlock <$> takeRest

reconstruct :: Maybe Text -> Maybe Text -> [Offset] -> [Piece] -> Text
reconstruct mBefore mAfter offsets = T.stripEnd . T.concat . f 0 offsets
    where
        after  = fromMaybe "" mAfter
        before = fromMaybe "" mBefore

        f :: Int -> [Offset] -> [Piece] -> [Text]
        f _ _ [] = []
        f _ [] ps = fmap pieceToText ps
        f pos delimOffsets (TextBlock t:ps) = t : f (T.length t + pos) delimOffsets ps
        f pos (Offset o:os) (Delim t: ps)   =
            let extraSpaces = o - pos
                toInsert = T.replicate extraSpaces " " <> before <> t <> after
             in toInsert : f (pos + extraSpaces +1 ) os ps

pieceToText :: Piece -> Text
pieceToText (TextBlock t) = t
pieceToText (Delim t)     = t

delimiterOffsets :: [Piece] -> [Offset]
delimiterOffsets = f 0
    where
        f :: Int -> [Piece] -> [Offset]
        f _ []                 = []
        f pos (TextBlock t:ss) = f (pos + T.length t) ss
        f pos (Delim t   :ss)  = Offset pos : f (pos + T.length t) ss

maxOffsets :: [[Offset]] -> [Offset]
maxOffsets os = case maxOffset of
    Just o  -> o : maxOffsets (rest o)
    Nothing -> []
    where
        advance :: Offset -> [Offset] -> [Offset]
        advance _ []                = []
        advance minOffset os'@(o:_) = fmap (+ (minOffset - o)) os'

        maxOffset :: Maybe Offset
        maxOffset = maximumMay . catMaybes $ fmap headMay os

        -- Move the rest of the offsets to the right so that all starts on the same column
        rest :: Offset -> [[Offset]]
        rest o = filter (not . null) $ fmap (tailSafe . advance o) os


align :: [Separator] -> [Text] -> [Text]
align s = alignApa (defaultOptions s)

alignApa :: AlignOptions -> [Text] -> [Text]
alignApa opts ts = fmap (reconstruct (prefix opts) (suffix opts) offsets) rows
    where
        rows :: [[Piece]]
        rows = parseRows (separators opts) ts

        offsets :: [Offset]
        offsets = maxOffsets $ fmap delimiterOffsets rows

parseRows :: [Separator] -> [Text] -> [[Piece]]
parseRows separators = rights . fmap (parseRow separators)

parseRow :: [Separator] -> Text -> Either (ParseError (Token Text) Void) [Piece]
parseRow separators = parse (lineParser separators) "parseRow"

