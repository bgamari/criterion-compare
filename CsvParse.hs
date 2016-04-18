{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module CsvParse (readResults) where

import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BSL

import Types

data BenchResult = BenchResult { benchName :: BenchName
                               , benchStats :: Stats
                               }

instance FromRecord BenchResult where
    parseRecord v
      | V.length v == 7 =
        bench <$> v .! 0
              <*> v .! 1
              <*> v .! 2
              <*> v .! 3
              <*> v .! 4
              <*> v .! 5
              <*> v .! 6
      | otherwise = empty
      where bench a b c d e f g = BenchResult a (Stats b c d e f g)

readResults :: FilePath -> IO (M.Map BenchName Stats)
readResults fname = do
    mxs <- parseResults <$> BSL.readFile fname
    case mxs of
      Left err -> fail err
      Right xs -> return $ M.fromList $ map (\(BenchResult a b) -> (a, b)) $ V.toList xs

parseResults :: BSL.ByteString -> Either String (V.Vector BenchResult)
parseResults =
    decode NoHeader
    . BSL.unlines
    . filter (not . ("Name,Mean" `BSL.isPrefixOf`))
    . BSL.lines
