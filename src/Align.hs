{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import           Control.Applicative
import           Data.Either
import qualified Data.List            as L
import           Data.Map             (Map)
import qualified Data.Map             as M
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

newtype ColNr = ColNr Int
    deriving (Show, Eq, Ord)

newtype Offset = Offset Int
    deriving (Show, Eq, Ord, Num)

newtype RowOffsets = RowOffsets [Offset]
    deriving (Show, Eq, Ord)

newtype Columns = Columns { unColumns :: [Text]}

align :: Separator -> [Text] -> [Text]
align sep ts = fmap (alignColumns sep offsets) cols
    where
        cols :: [Columns]
        cols = fmap (getColumns sep) ts

        offsets :: [Offset]
        offsets = columnSizes cols

getColumns :: Separator -> Text -> Columns
getColumns sep = Columns . T.splitOn (unSeparator sep)

columnSizes :: [Columns] -> [Offset]
columnSizes = fmap snd
            . L.sort
            . M.toList
            . M.unionsWith max
            . fmap colSize

colSize :: Columns -> Map ColNr Offset
colSize = M.fromList
        . zipWith (\nr o -> (ColNr nr, Offset o)) [1..]
        . fmap T.length
        . unColumns

alignColumns :: Separator -> [Offset] -> Columns -> Text
alignColumns (Separator sep) offsets (Columns cols) =
    T.intercalate sep $ zipWith combine offsets cols
    where
        combine :: Offset -> Text -> Text
        combine (Offset o) t = t <> T.pack (take (o - T.length t) (repeat ' '))


------------ megaparsec stuffs ------------

data Stuff = Whatever Text | Delim Text deriving (Show, Eq, Ord)

lineParser :: [Separator] -> Parsec Void Text [Stuff]
lineParser ss = do
    parts <- many
           . foldl1 (<|>)
           $ fmap (try . delimP) ss <> [try $ textChunk ss]
    lastPart <- afterLast
    pure $ parts <> [lastPart]

delimP :: Separator -> Parsec Void Text Stuff
delimP = fmap Delim . string' . unSeparator

textChunk :: [Separator] -> Parsec Void Text Stuff
textChunk ss = fmap (Whatever . T.pack) $  manyTill anyChar findDelim
    where
        findDelim = lookAhead . foldl1 (<|>) $ fmap delimP ss <> []

afterLast :: Parsec Void Text Stuff
afterLast = Whatever <$> takeRest

reconstruct :: [Offset] -> [Stuff] -> Text
reconstruct offsets = T.concat . f 0 offsets
    where
        f :: Int -> [Offset] -> [Stuff] -> [Text]
        f _ _ [] = []
        f _ [] ps = fmap stuffToText ps
        f pos delimOffsets (Whatever t:ps) = t : f (T.length t + pos) delimOffsets ps
        f pos (Offset o:os) (Delim t: ps)   =
            let extraSpaces = o - pos
                toInsert = T.replicate extraSpaces " " <> t
             in toInsert : f (pos + T.length toInsert) os ps

stuffToText :: Stuff -> Text
stuffToText (Whatever t) = t
stuffToText (Delim t)    = t

delimiterOffsets :: [Stuff] -> [Offset]
delimiterOffsets = f 0
    where
        f :: Int -> [Stuff] -> [Offset]
        f _ []                = []
        f pos (Whatever t:ss) = f (pos + T.length t) ss
        f pos (Delim t   :ss) = Offset pos : f (pos + T.length t) ss

maxOffsets :: [[Offset]] -> [Offset]
maxOffsets = f (Offset 0)
    where
        f :: Offset -> [[Offset]] -> [Offset]
        f _ [] = []
        f pos (os:oss) =
            let maxOffset :: Maybe Offset
                maxOffset = maximumMay . catMaybes $ fmap headMay os

                apa :: Offset -> [Offset] -> [Offset]
                apa _ []          = []
                apa minO os@(o:_) = fmap (+ (minO - o)) os

             in maybe [] (\o -> o : f o (fmap (apa o) oss)) maxOffset

align' :: [Separator] -> [Text] -> [Text]
align' separators ts = undefined
    where
        rows :: [[Stuff]]
        rows = rights $ fmap (parse (lineParser separators) "") ts

        rowOffsets :: [[Offset]]
        rowOffsets = fmap delimiterOffsets rows


-- Adding afterLast to lineParsers makes the parser infinite :(
-- > parse (lineParser [Separator "->"]) "" ".a.b ->c.hejsan->hej"
