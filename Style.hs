{-# LANGUAGE OverloadedStrings #-}
module Style (style) where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Clay hiding (round, style)

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
        star # byClass (T.pack $ "stat-p"++show (round n)) ? do backgroundColor $ hsl 0 (shadeSat $ n/8) 90
        star # byClass (T.pack $ "stat-n"++show (round n)) ? do backgroundColor $ hsl 128 (shadeSat $ n/8) 90

shadeSat n = 30 + 60*n
