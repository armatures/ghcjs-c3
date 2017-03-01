{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module C3.Types where

import           Data.Aeson
import           Data.Text   (Text)
import           Data.Text (pack, toLower)
import           GHCJS.Types (JSVal)
import           Data.Vector (fromList)

-- | A reference to the chart object returned by C3.
newtype Chart = Chart { getChart :: JSVal }
newtype DataIndex = DataIndex { ids :: Text } deriving ToJSON
newtype Columns = Columns { columns :: [Column] }

-- | The definition of the chart we want to generate.
data ChartOptions = ChartOptions
  { -- | The CSS selector the chart will be set to.
    bindTo    :: Text
    -- | The data sourcing the chart.
  , chartData :: ChartData
  , chartSizeOptions :: Maybe ChartSizeOptions
  }

data ChartSizeOptions
  = ChartSizeOptions
  { _chart_size_height :: Int
  }

-- | The data source for our chart.
data ChartData = ChartData
  { -- | The chart type.
    chartType    :: ChartType
    -- | The columns of data.
  , chartColumns :: [Column]
  }

data GaugeOpts
  = GaugeOpt
  { _gauge_min :: Int
  , _gauge_max :: Int
  , _gauge_units :: Text
  , _gauge_width :: Int
  } deriving Show

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
  | Gauge GaugeOpts
  deriving Show

instance ToJSON ChartOptions where
  toJSON ChartOptions{..} = object [
    "bindto" .= bindTo,
    "data"   .= chartData,
    header .= chartType chartData]
    where
      header = case chartType chartData of
        (Gauge _) -> "gauge"
        g -> toLower $ pack $ show g

instance ToJSON ChartSizeOptions where
  toJSON ChartSizeOptions{..} = object [
    "height" .= _chart_size_height]

instance ToJSON ChartData where
  toJSON ChartData{..} = object [
    "type"    .= dumbType chartType,
    "columns" .= chartColumns ]
    where
      dumbType :: ChartType -> Text
      dumbType (Gauge _) = "gauge"
      dumbType o = toLower $ pack $ show o

instance ToJSON Column where
  toJSON Column{..} =
    Array (fromList (String columnName : map toJSON columnValues))

instance ToJSON GaugeOpts where
  toJSON GaugeOpt{..} = object [
      "min"   .= _gauge_min
    , "max"   .= _gauge_max
    , "units" .= _gauge_units
    , "width" .= _gauge_width
    ]

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
  toJSON (Gauge g)  = object [ "gauge" .= toJSON g]

instance ToJSON Columns where
  toJSON (Columns c) = object [
    "columns" .= toJSON c
    ]
