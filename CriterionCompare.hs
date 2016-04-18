{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad (forM_)
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as BSL
import Lucid
import Numeric

-- | The name of a set of benchmark results from a single run.
newtype RunName = RunName String
                deriving (Eq, Ord, Show, FromField)

-- | The name of a benchmark
newtype BenchName = BenchName String
                  deriving (Eq, Ord, Show, FromField)

data Stats = Stats { statsMean, statsMeanLB, statsMeanUB :: Double
                   , statsStd, statsStdLB, statsStdUB :: Double
                   }

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

readResults :: FilePath -> IO [BenchResult]
readResults fname = do
    mxs <- parseResults <$> BSL.readFile fname
    case mxs of
      Left err -> fail err
      Right xs -> return $ V.toList xs

parseResults :: BSL.ByteString -> Either String (V.Vector BenchResult)
parseResults =
    decode NoHeader
    . BSL.unlines
    . filter (not . ("Name,Mean" `BSL.isPrefixOf`))
    . BSL.lines

invert :: M.Map RunName [BenchResult] -> M.Map BenchName (M.Map RunName Stats)
invert runs =
    M.unionsWith M.union [ M.singleton (benchName result) $ M.singleton run (benchStats result)
                         | (run, results) <- M.toList runs
                         , result <- results ]

tabulate :: M.Map BenchName (M.Map RunName Stats) -> Html ()
tabulate results =
    table_ $ do
        tr_ $ do
            th_ "Benchmark"
            forM_ (M.keys $ head $ M.elems results) $ \(RunName runName) -> do
                th_ $ toHtml runName

        forM_ (M.assocs results) $ \(BenchName benchName, runs) -> tr_ $ do
            td_ $ toHtml benchName
            forM_ (M.assocs runs) $ \(RunName runName, stats) -> do
                let mean = showGFloat (Just 2) (statsMean stats) ""
                    std = showString " ± " $ showGFloat (Just 2) (statsStd stats) ""
                td_ $ do
                    toHtml mean
                    span_ [class_ "stddev"] $ toHtml std

main :: IO ()
main = do
    results <- sequence [ M.singleton (RunName "old") <$> readResults "old.csv"
                        , M.singleton (RunName "new") <$> readResults "new.csv"
                        ]
    renderToFile "hi.html" $ doctypehtml_ $ do
        head_ $ do
            title_ "Criterion comparison"
            meta_ [ charset_ "UTF-8" ]
            style_ ".stddev { font-size: small; } td { padding: 0 1em; }"
        body_ $ tabulate $ invert $ M.unions results
