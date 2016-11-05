{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C3.Types where

import           Data.Aeson
import           Data.Text   (Text)
import           GHCJS.Types (JSVal)
import           Data.Vector (fromList)

-- | A reference to the chart object returned by C3.
newtype Chart = Chart { getChart :: JSVal }

-- | The definition of the chart we want to generate.
data ChartOptions = ChartOptions
  { -- | The CSS selector the chart will be set to.
    bindTo    :: Text
    -- | The data sourcing the chart.
  , chartData :: ChartData
  }

-- | The data source for our chart.
data ChartData = ChartData
  { -- | The chart type.
    chartType    :: ChartType
    -- | The columns of data.
  , chartColumns :: [Column]
  }

-- | A column of data to source our chart.
data Column = Column
  { -- | The datum name.
    columnName   :: Text
    -- | The associated data values.
  , columnValues :: [Double]
  }

-- | The type of chart.
data ChartType
  = Line
  | Spline
  | Step
  | Area
  | AreaSpline
  | AreaStep
  | Bar
  | Scatter
  | Pie
  | Donut
  | Gauge

instance ToJSON ChartOptions where
  toJSON ChartOptions{..} = object [
    "bindto" .= bindTo,
    "data"   .= chartData ]

instance ToJSON ChartData where
  toJSON ChartData{..} = object [
    "type"    .= chartType,
    "columns" .= chartColumns ]

instance ToJSON Column where
  toJSON Column{..} = 
    Array (fromList (String columnName : map toJSON columnValues))

instance ToJSON ChartType where
  toJSON Line       = String "line"
  toJSON Spline     = String "spline"
  toJSON Step       = String "step"
  toJSON Area       = String "area"
  toJSON AreaSpline = String "area-spline"
  toJSON AreaStep   = String "area-step"
  toJSON Bar        = String "bar"
  toJSON Scatter    = String "scatter"
  toJSON Pie        = String "pie"
  toJSON Donut      = String "donut"
  toJSON Gauge      = String "gauge"
