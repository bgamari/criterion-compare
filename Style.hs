{-# LANGUAGE OverloadedStrings #-}
module Style (style) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Clay hiding (round, style, min)

style :: T.Text
style = TL.toStrict $ render style'

style' :: Css
style' = do
    ".stddev" ? do
        fontSizeCustom xSmall
    "td" ? do
        paddingLeft (em 1)
        paddingRight (em 1)

    forM_ [0..10] $ \n -> do
        star # byClass (T.pack $ "stat-p"++show (round n)) ? do backgroundColor $ shade 0 n
        star # byClass (T.pack $ "stat-n"++show (round n)) ? do backgroundColor $ shade 128 n

-- start by increasing saturation up to sMax then decreasing lightness from
-- lMax; I think this makes the colors easier to see:
shade h n = hsl h (min sMax n10) (min lMax (lMax - (n10-sMax)))
    where n10 = n*10
          lMax = 90
          sMax = 75
