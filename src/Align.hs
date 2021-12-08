{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import           Control.Applicative
-- import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.String                    ( IsString )
import           Data.Text                      ( Text )
import qualified Data.Text                                    as T
import           Data.Void
import           Prelude
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
    { prefix      :: Maybe Text
    , suffix      :: Maybe Text
    , stripBefore :: Bool
    , stripAfter  :: Bool
    , separators  :: [Separator]
    }
    deriving Show

defaultOptions :: [Separator] -> AlignOptions
defaultOptions = AlignOptions Nothing Nothing False False

lineParser :: AlignOptions -> Parsec Void Text [Piece]
lineParser opts = do
    parts    <- many . foldl1 (<|>) $ fmap (try . delimP) ss <> [try $ textChunk opts]
    lastPart <- afterLast
    pure . fmap (stripParts opts) $ parts <> [lastPart, Delim ""]
    where ss = separators opts

stripParts :: AlignOptions -> Piece -> Piece
stripParts _ p@(Delim _) = p
stripParts opts (TextBlock t) =
    TextBlock
        . (if stripBefore opts then T.stripStart else id)
        . (if stripAfter opts then T.stripEnd else id)
        $ t

delimP :: Separator -> Parsec Void Text Piece
delimP = fmap Delim . string' . unSeparator

textChunk :: AlignOptions -> Parsec Void Text Piece
textChunk opts = fmap (TextBlock . T.pack) $ manyTill anySingle findDelim
  where
    ss        = separators opts
    findDelim = lookAhead . foldl1 (<|>) $ fmap delimP ss <> []

afterLast :: Parsec Void Text Piece
afterLast = TextBlock <$> takeRest

reconstruct :: AlignOptions -> [[Piece]] -> [Text]
reconstruct opts rows = fmap (T.stripEnd . T.concat . toSizedChunks 0 offsets) rows
  where
        -- Turn the pieces into a list of text chunk of the correct size
    toSizedChunks :: Int -> [Offset] -> [Piece] -> [Text]
    toSizedChunks _ _  [] = []
    toSizedChunks _ [] ps = fmap pieceToText ps
    toSizedChunks pos delimOffsets (TextBlock t : ps) =
        t : toSizedChunks (T.length t + pos) delimOffsets ps
    toSizedChunks pos (Offset o : os) (Delim t : ps) =
        let -- Make sure the column starts at the right place
            leadingSpaces  = T.replicate (o - pos) " "
            -- Fill with spaces if operator is shorter than the longest
            trailingSpaces = T.replicate (maxDelimLength - delimLength) " "
            delimLength    = T.length t
            toInsert       = leadingSpaces <> before <> t <> trailingSpaces <> after
        in  toInsert : toSizedChunks (pos + T.length toInsert) os ps

    maxDelimLength :: Int
    maxDelimLength = foldl max 0 (fmap (T.length . unSeparator) $ separators opts)

    offsets :: [Offset]
    offsets = maxOffsets $ fmap (delimiterOffsets maxDelimLength) rows

    after :: Text
    after = fromMaybe "" $ suffix opts

    before :: Text
    before = fromMaybe "" $ prefix opts

pieceToText :: Piece -> Text
pieceToText (TextBlock t) = t
pieceToText (Delim     t) = t

-- | Calculate the offset of each text chunk, not including the width of the delimiter
delimiterOffsets :: Int -> [Piece] -> [Offset]
delimiterOffsets maxDelimLength = f 0
  where

    f :: Int -> [Piece] -> [Offset]
    f _   []                 = []
    f pos (TextBlock t : ss) = f (pos + T.length t) ss
    f pos (Delim     _ : ss) = Offset pos : f (pos + maxDelimLength) ss

maxOffsets :: [[Offset]] -> [Offset]
maxOffsets os = case maxOffset of
    Just o  -> o : maxOffsets (rest o)
    Nothing -> []
  where
    advance :: Offset -> [Offset] -> [Offset]
    advance _         []          = []
    advance minOffset os'@(o : _) = fmap (+ (minOffset - o)) os'

    maxOffset :: Maybe Offset
    maxOffset = maximumMay . catMaybes $ fmap headMay os

    -- Move the rest of the offsets to the right so that all starts on the same column
    rest :: Offset -> [[Offset]]
    rest o = filter (not . null) $ fmap (tailSafe . advance o) os


align :: AlignOptions -> [Text] -> [Text]
align opts = reconstruct opts . parseRows opts

parseRows :: AlignOptions -> [Text] -> [[Piece]]
parseRows opts = rights . fmap (parseRow opts)

parseRow :: AlignOptions -> Text -> Either (ParseErrorBundle Text Void) [Piece]
parseRow opts = parse (lineParser opts) "parseRow"
