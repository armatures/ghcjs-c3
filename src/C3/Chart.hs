{-# LANGUAGE OverloadedStrings #-}

module C3.Chart where

import Data.Text (Text)
import Data.Default
import C3.Types
import Control.Lens

chartOpts :: ChartOptions
chartOpts = def & bindTo .~ "my-chart"
                & chartData .~ def
                & chartSizeOptions .~ Nothing

fromKeyValue :: [(Text, Double)] -> Datum
fromKeyValue = Columns . map (\(k,v) -> Column k [v])
