{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Control.Applicative
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, dropExtension, (<.>))
import Numeric
import Lucid
import Graphics.Rendering.Chart (toRenderable)
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import Data.Default
import Options.Applicative

import Style
import Plot
import CsvParse
import Types

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
            cls = T.pack $ "stat-"++sign++show (abs n)
              where sign = if rel > 0 then "p" else "n"
                    n = min 10 $ max (-10) $ round $ rel / 0.05 :: Int
        in span_ [class_ cls] $ toHtml $ showGFloat (Just 1) (100*rel) "%"
      | otherwise
      = showAbs stats

    showAbs stats = toHtml $ showGFloat (Just 2) (statsMean stats) ""

data Options = Options { optRunNames :: [RunName]
                       , optOutput   :: FilePath
                       , optRunPaths :: [FilePath]
                       }

options :: Options.Applicative.Parser Options
options =
    Options <$> many (option (RunName <$> str) $ short 'l' <> long "label" <> help "label")
            <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "output file name" <> value "comparison")
            <*> many (argument str $ metavar "FILE" <> help "CSV file name")

main :: IO ()
main = do
    Options{..} <- execParser $ info (helper <*> options) mempty
    results <- sequence [ (name',) <$> readResults path
                        | (name, path) <- zip (map Just optRunNames ++ repeat Nothing) optRunPaths
                        , let name' = fromMaybe (RunName $ dropExtension $ takeFileName path) name
                        ]

    renderableToFile def (optOutput <.> "svg") $ toRenderable $ plot $ M.fromList results
    --let table = tabulateAbsolute $ invert $ M.unions results
    let table = tabulateRelative (fst $ head results) $ invert $ M.fromList results

    renderToFile (optOutput <.> "html") $ doctypehtml_ $ do
        head_ $ do
            title_ "Criterion comparison"
            meta_ [ charset_ "UTF-8" ]
            style_ style
        body_ $ toTable table
