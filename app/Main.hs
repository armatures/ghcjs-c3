{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad      (forever, forM_)

import C3

main :: IO ()
main = do
  js_createChartContainer "chart"
  chart <- generate opts
  forever $ do
    forM_ [Area, Pie, Donut, Gauge, Step] $ \typ -> do
      threadDelay (2 * second)
      transform chart typ
  where
    opts = ChartOptions "#chart" $
             ChartData Bar $
               [ Column "data1" [30, 200, 100, 400, 150, 250]
               , Column "data2" [50, 20, 10, 40, 15, 25]
               ]
    second = 1000000
