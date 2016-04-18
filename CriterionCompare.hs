{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict as M
import qualified Data.Text as T
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

invert :: (Ord a, Ord b) => M.Map a (M.Map b v) -> M.Map b (M.Map a v)
invert runs =
    M.unionsWith M.union [ M.singleton bench $ M.singleton run stats
                         | (run, results) <- M.assocs runs
                         , (bench, stats) <- M.assocs results ]

toTable :: M.Map BenchName (M.Map RunName (Html ())) -> Html ()
toTable results =
    table_ $ do
        tr_ $ do
            th_ "Benchmark"
            forM_ (M.keys $ head $ M.elems results) $ \(RunName runName) ->
                th_ $ toHtml runName

        forM_ (M.assocs results) $ \(BenchName benchName, runs) -> tr_ $ do
            td_ $ toHtml benchName
            forM_ (M.assocs runs) $ \(RunName runName, content) -> td_ content

tabulateAbsolute :: M.Map BenchName (M.Map RunName Stats)
                 -> M.Map BenchName (M.Map RunName (Html ()))
tabulateAbsolute = fmap $ fmap cell
  where
    cell stats =
      let mean = showGFloat (Just 2) (statsMean stats) ""
          std = showString " ± " $ showGFloat (Just 2) (statsStd stats) ""
      in td_ $ do
          toHtml mean
          span_ [class_ "stddev"] $ toHtml std

tabulateRelative :: RunName -> M.Map BenchName (M.Map RunName Stats)
                 -> M.Map BenchName (M.Map RunName (Html ()))
tabulateRelative refRun results =
    M.mapWithKey (\bench -> M.mapWithKey (cell bench)) results
  where
    cell :: BenchName -> RunName -> Stats -> Html ()
    cell bench run stats
      | run == refRun
      = showAbs stats
      | Just refStats <- M.lookup bench results >>= M.lookup refRun
      = let rel = (statsMean stats - statsMean refStats) / statsMean refStats
            cls = T.pack $ "stat-"++sign++show n
              where sign = if rel > 0 then "p" else "n"
                    n = min 10 $ max (-10) $ round $ 10*rel :: Int
        in span_ [class_ cls] $ toHtml $ showGFloat (Just 1) (100*rel) "%"
      | otherwise
      = showAbs stats

    showAbs stats = toHtml $ showGFloat (Just 2) (statsMean stats) ""

main :: IO ()
main = do
    results <- sequence [ M.singleton (RunName "old") <$> readResults "old.csv"
                        , M.singleton (RunName "new") <$> readResults "new.csv"
                        ]
    --let table = tabulateAbsolute $ invert $ M.unions results
    let table = tabulateRelative (RunName "old") $ invert $ M.unions results 

    renderToFile "hi.html" $ doctypehtml_ $ do
        head_ $ do
            title_ "Criterion comparison"
            meta_ [ charset_ "UTF-8" ]
            style_ ".stddev { font-size: small; } td { padding: 0 1em; }"
        body_ $ toTable table
