{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forever, forM_)

import C3

main :: IO ()
main = do
  js_createChartContainer "chart"
  js_createChartContainer "gauge-chart"
  chart <- generate opts
  gaugeChart <- generate gaugeChart
  loadData gaugeChart $ gaugeColumn "whatever" 80.9 gaugeOpts
  threadDelay (2 * second)
  _newChart <- loadData gaugeChart $ gaugeColumn "whatever" 25.9  gaugeOpts
  forever $ do
    forM_ [Area, Pie, Donut, (Gauge gaugeOpts), Step] $ \typ -> do
      threadDelay (2 * second)
      transform chart typ
  where
    opts = ChartOptions "#chart" chartDatas Nothing
    second = 1000000
    gaugeChart = ChartOptions "#gauge-chart" gaugeData $
             Just $ ChartSizeOptions 280


chartDatas = ChartData Bar
               [ Column "data1" [30, 200, 100, 400, 150, 250]
               , Column "data2" [50, 20, 10, 40, 15, 25]
               ]
gaugeData = ChartData (Gauge gaugeOpts)
               [ Column "whatever" [25.3]
               ]

gaugeOpts = GaugeOpt 0 100 "PERCENT" 138

gaugeColumn title val opts =  ChartData (Gauge opts)
                          [ Column title [val] ]
