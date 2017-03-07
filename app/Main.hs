{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forever, forM_)

import C3

main :: IO ()
main = do
  js_createChartContainer "chart"
  js_createChartContainer "gauge-chart"
  js_createChartContainer "pie-chart"
  chart <- generate opts
  gaugeChart <- generate gaugeChartOpt
  pieChart <- generate pieChartOpt
  threadDelay (2 * second)
  _ <- loadData gaugeChart $ Columns [ Column "whatever" [80.9] ]
  threadDelay (2 * second)
  _ <- loadData pieChart pieData2
  forever $ do
    forM_ [Area, Pie, Donut, Step] $ \typ -> do
      threadDelay (2 * second)
      transform chart typ
  where
    second = 1000000
    opts = ChartOptions "#chart" chartDatas Nothing
    chartDatas = ChartData Bar mainColumns
    gaugeChartOpt = ChartOptions "#gauge-chart" gaugeData largerChart
    gaugeData = ChartData (Gauge gaugeOpts) gaugeColumns1

    pieChartOpt = ChartOptions "#pie-chart" (ChartData Pie pieData) Nothing

gaugeColumns1 :: Datum
gaugeColumns1 = Columns
  [ Column "whatever" [25.3]]

mainColumns :: Datum
mainColumns = Columns
  [ Column "data1" [30, 200, 100, 400, 150, 250]
  , Column "data2" [50, 20, 10, 40, 15, 25]
  ]

largerChart :: Maybe ChartSizeOptions
largerChart = Just $ ChartSizeOptions 280

pieData :: Datum
pieData = Rows ["US", "Them"] [[60,40]]

pieData2 :: Datum
pieData2 = Rows ["US", "Them", "something", "Gary Busey"] [[10,5,15,70]]

gaugeOpts :: GaugeOpts
gaugeOpts = GaugeOpt 0 100 "PERCENT" 138
