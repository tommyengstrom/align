{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import           Control.Applicative
import qualified Data.List            as L
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Monoid
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

tailSafe :: [a] -> [a]
tailSafe []     = []
tailSafe (_:xs) = xs


newtype Separator = Separator {unSeparator :: Text}
    deriving (Show, IsString)

newtype ColNr = ColNr Int
    deriving (Show, Eq, Ord)

newtype Offset = Offset Int
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
lineParser ss = many
              . foldl1 (<|>)
              $ fmap (try . delimP) ss <> [try $ textChunk ss] -- , afterLast]

delimP :: Separator -> Parsec Void Text Stuff
delimP = fmap Delim . string' . unSeparator

textChunk :: [Separator] -> Parsec Void Text Stuff
textChunk ss = fmap (Whatever . T.pack) $  manyTill anyChar findDelim
    where
        findDelim = lookAhead . foldl1 (<|>) $ fmap delimP ss <> []

afterLast :: Parsec Void Text Stuff
afterLast = Whatever <$> takeRest


-- Adding afterLast to lineParsers makes the parser infinite :(
-- > parse (lineParser [Separator "->"]) "" ".a.b ->c.hejsan->hej"
