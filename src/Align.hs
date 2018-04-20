{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import           Control.Applicative
-- import           Control.Monad
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
    { prefix      :: Maybe Text
    , suffix      :: Maybe Text
    , stripBefore :: Bool
    , stripAfter  :: Bool
    , separators  :: [Separator]
    } deriving Show

defaultOptions :: [Separator] -> AlignOptions
defaultOptions = AlignOptions Nothing Nothing False False

lineParser :: AlignOptions -> Parsec Void Text [Piece]
lineParser opts = do
    parts <- many
           . foldl1 (<|>)
           $ fmap (try . delimP) ss <> [try $ textChunk opts]
    lastPart <- afterLast
    pure . fmap (stripParts opts) $ parts <> [lastPart, Delim ""]
    where
        ss = separators opts

stripParts :: AlignOptions -> Piece -> Piece
stripParts _ p@(Delim _) = p
stripParts opts (TextBlock t) =
    TextBlock . (if stripBefore opts then T.stripStart else id)
              . (if stripAfter opts then T.stripEnd else id)
              $ t

delimP :: Separator -> Parsec Void Text Piece
delimP = fmap Delim . string' . unSeparator

textChunk :: AlignOptions -> Parsec Void Text Piece
textChunk opts = fmap (TextBlock . T.pack) $ manyTill anyChar findDelim
    where
        ss = separators opts
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


align :: AlignOptions -> [Text] -> [Text]
align opts ts = fmap (reconstruct (prefix opts) (suffix opts) offsets) rows
    where
        rows :: [[Piece]]
        rows = parseRows opts ts

        offsets :: [Offset]
        offsets = maxOffsets $ fmap delimiterOffsets rows

parseRows :: AlignOptions -> [Text] -> [[Piece]]
parseRows opts = rights . fmap (parseRow opts)

parseRow :: AlignOptions -> Text -> Either (ParseError (Token Text) Void) [Piece]
parseRow opts = parse (lineParser opts) "parseRow"

