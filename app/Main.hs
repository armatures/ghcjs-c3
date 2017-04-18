{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forever, forM_)
import Data.Default
import Control.Lens hiding (transform)

import C3
import C3.Chart.Gauge
import C3.Chart

main :: IO ()
main = do
  js_createChartContainer "gauge-chart"
  js_createChartContainer "pie-chart"
  js_createChartContainer "chart"
  gaugeChart <- generate $ def & bindTo .~ "gauge-chart"
                               & chartData .~ gaugeData
                               & chartSizeOptions .~ largerChart
  pieChart <- generate $ def & bindTo .~ "pie-chart"
                             & chartData . chartDatum .~ pieData
  threadDelay (2 * second)
  _ <- load gaugeChart $ Columns [ Column "whatever" [80.9] ]
  threadDelay (2 * second)
  _ <- load pieChart pieData2
  chart <- generate $ def & bindTo .~ "chart"
                          & chartData . chartDatum .~ mainColumns
  forever $ do
    forM_ [Area, Pie, Donut, Step] $ \typ -> do
      threadDelay (2 * second)
      transform chart typ
  where
    second = 1000000
    gaugeData = def & chartType .~ Gauge gaugeOpts
                    & chartDatum .~ gaugeColumns1

gaugeColumns1 :: Datum
gaugeColumns1 = Columns
  [ Column "whatever" [25.3]]

mainColumns :: Datum
mainColumns = Columns
  [ Column "data1" [30, 200, 100, 400, 150, 250]
  , Column "data2" [50, 20, 10, 40, 15, 25]
  ]

largerChart :: Maybe ChartSizeOptions
largerChart = Just $ def { _chartHeight = Just 250 }

pieData :: Datum
pieData = Columns [ Column "US" [60], Column "Them" [40]]

pieData2 :: Datum
pieData2 = fromKeyValue $ zip ["US", "Them", "something", "Gary Busey"] [10,5,4,8]

gaugeOpts :: GaugeOpts
gaugeOpts = def { gaugeWidth = Just 25 }
