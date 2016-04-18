{-# LANGUAGE RecordWildCards #-}

module Plot where

import qualified Data.Map as M

import Graphics.Rendering.Chart
import Data.Default
import Data.List
import Control.Lens
import Types
import Data.Colour
import qualified Data.Colour.Names as N

plot :: M.Map RunName (M.Map BenchName Stats) -> Layout PlotIndex Double
plot results = layout
  where
    idxs' :: [(PlotIndex, Maybe (BenchName, RunName))]
    idxs' = addIndexes
          $ intercalate (replicate 3 Nothing)
          $ transpose
            [ [ Just (benchName, runName)
              | (benchName, _stats) <- M.assocs runs
              ]
            | (runName, runs) <- M.assocs results
            ]

    idxs :: M.Map (RunName, BenchName) PlotIndex
    idxs = M.unions [ M.singleton (runName, benchName) idx
                    | (idx, Just (benchName, runName)) <- idxs'
                    ]

    plotRun :: AlphaColour Double
            -> (RunName, M.Map BenchName Stats)
            -> PlotErrBars PlotIndex Double
    plotRun color (runName, benchmarks) =
          plot_errbars_title .~ getRunName runName
        $ plot_errbars_line_style . line_color .~ color
        $ plot_errbars_values .~ [ ErrPoint ex ey
                                 | (benchName, Stats{..}) <- M.assocs benchmarks
                                 , let Just idx = M.lookup (runName, benchName) idxs
                                 , let ex = ErrValue idx idx idx
                                 , let ey = ErrValue statsMeanLB statsMean statsMeanUB
                                 ]
        $ def

    colors :: [AlphaColour Double]
    colors = map opaque $ cycle [N.red, N.blue, N.purple, N.yellow, N.brown, N.green, N.cyan]

    plots :: [Plot PlotIndex Double]
    plots = map toPlot $ zipWith plotRun colors (M.assocs results)

    labels = map (\(BenchName name, idx) -> (idx, name)) $ M.assocs $ M.mapKeys snd idxs

    layout :: Layout PlotIndex Double
    layout = layout_title .~ "Criterion comparison"
           $ layout_plots .~ plots
           $ layout_x_axis . laxis_override .~ (axis_labels .~ [labels])
           $ def
