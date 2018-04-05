{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import Text.Parsec
-- import Control.Applicative
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
import Data.Monoid

tailSafe :: [a] -> [a]
tailSafe [] = []
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
--        . flip mappend [0]       -- count the last column
        . fmap T.length -- Length
        . unColumns

alignColumns :: Separator -> [Offset] -> Columns -> Text
alignColumns (Separator sep) offsets (Columns cols) =
    T.intercalate sep $ zipWith combine offsets cols
    where
        combine :: Offset -> Text -> Text
        combine (Offset o) t = t <> T.pack (take (o - T.length t) (repeat ' '))

