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
import Lucid.Base
import Graphics.Rendering.Chart (toRenderable)
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile)
import Data.Default
import Options.Applicative hiding (style)

import Style
import Plot
import CsvParse
import Types

invert :: (Ord a, Ord b) => M.Map a (M.Map b v) -> M.Map b (M.Map a v)
invert runs =
    M.unionsWith M.union [ M.singleton bench $ M.singleton run stats
                         | (run, results) <- M.assocs runs
                         , (bench, stats) <- M.assocs results ]

toTable :: [(BenchName,Int)] -> M.Map BenchName (M.Map RunName (Either (Html ()) (Attribute,Double))) -> Html ()
toTable orderOrig results =
    table_ $ do
      thead_ $
        tr_ $ do
            th_ "Benchmark"
            forM_ (M.keys $ head $ M.elems results) $ \(RunName runName) ->
                th_ $ toHtml runName

      -- for list.js
      tbody_ [class_ "list"] $
        forM_ (M.assocs results) $ \(bn@(BenchName benchName), runs) -> tr_ $ do
            let o = fromMaybe maxBound $ lookup bn orderOrig
            td_ [class_ "benchName"] $ toHtml benchName
            forM_ (M.assocs runs) $ \(RunName runName, ec) -> do
              let content = either id (\(cls,n) -> span_ [cls] $ toHtml $ showGFloat (Just 1) n "%") ec
              td_ [class_ $ T.pack runName] content
            let significance = sum $ map (either (const 0) (abs . snd)) $ M.elems runs
            -- hidden tds to let us sort by original benchmark order:
            td_ [class_ "orderOrig", style_ "display:none;"] (toHtml $ show o)
            td_ [class_ "significance", style_ "display:none;"]
              -- subtract as stupid hack to get reverse order on first click
              (toHtml $ show (99999 - significance))
            -- TODO "largest regression", "largest improvement", etc. Then we
            --      must add those names to the JS snippet at the bottom

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
                 -> M.Map BenchName (M.Map RunName (Either (Html ()) (Attribute,Double)))
tabulateRelative refRun results =
    M.mapWithKey (\bench -> M.mapWithKey (cell bench)) results
  where
    cell bench run stats
      | run == refRun
      = showAbs stats
      | Just refStats <- M.lookup bench results >>= M.lookup refRun
      = let rel = (statsMean stats - statsMean refStats) / statsMean refStats
            cls = T.pack $ "stat-"++sign++show (abs n)
              where sign = if rel > 0 then "p" else "n"
                    n = min 10 $ max (-10) $ round $ rel / 0.025 :: Int
        -- in span_ [class_ cls] $ toHtml $ showGFloat (Just 1) (100*rel) "%"
        in Right (class_ cls, 100*rel)
      | otherwise
      = showAbs stats

    showAbs stats = Left $ toHtml $ showGFloat (Just 2) (statsMean stats) ""

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
    results <- sequence [ (name',) . M.fromList <$> readResults path
                        | (name, path) <- zip (map Just optRunNames ++ repeat Nothing) optRunPaths
                        , let name' = fromMaybe (RunName $ dropExtension $ takeFileName path) name
                        ]

    orderOrig <- zipWith (\i (nm,_)-> (nm,i)) [0..] <$> (readResults $ head optRunPaths)
    renderableToFile def (optOutput <.> "svg") $ toRenderable $ plot $ M.fromList results
    --let table = tabulateAbsolute $ invert $ M.unions results
    let table = tabulateRelative (fst $ head results) $ invert $ M.fromList results

    renderToFile (optOutput <.> "html") $ doctypehtml_ $ do
        head_ $ do
            title_ "Criterion comparison"
            meta_ [ charset_ "UTF-8" ]
            style_ style
        body_ $
          -- for list.js:
          div_ [id_ "bench"] $ do
            input_ [class_ "search", placeholder_ "Filter by name"]
            span_ "Sort by: "
            button_ [class_ "sort", makeAttribute "data-sort" "orderOrig"]    "original order"
            button_ [class_ "sort", makeAttribute "data-sort" "significance"] "significance"
            button_ [class_ "sort", makeAttribute "data-sort" "benchName"]    "name"
            toTable orderOrig table
        -- http://listjs.com :
        script_ [ src_ "http://cdnjs.cloudflare.com/ajax/libs/list.js/1.5.0/list.min.js"] (""::T.Text)
        script_ "new List(\"bench\", {valueNames: [\"orderOrig\", \"benchName\",\"significance\"]});"
