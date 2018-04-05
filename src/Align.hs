{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Align where

-- import Text.Parsec
-- import Control.Applicative
import Data.Text (Text)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)

newtype Separator = Separator {unSeparator :: Text}
    deriving (Show, IsString)

newtype ColNr = ColNr Int
    deriving (Show, Eq, Ord)

newtype Offset = Offset Int
    deriving (Show, Eq, Ord)

newtype Columns = Columns [Text]

align :: Separator -> [Text] -> [Text]
align sep ts = const ts sep

columnSizes :: Separator -> [Text] -> Map ColNr Offset
columnSizes sep = M.unionsWith max . fmap (colSize sep)

getColumns :: Separator -> Text -> Columns
getColumns sep = Columns . T.splitOn (unSeparator sep)

colSize :: Separator -> Text -> Map ColNr Offset
colSize sep = M.fromList
            . zipWith (\nr o -> (ColNr nr, Offset o)) [1..]
            . scanl (+) 0            -- total offset per column
            . flip mappend [0]      -- count the last column
            . fmap (succ . T.length) -- Length plus separator
            . T.splitOn (unSeparator sep)
