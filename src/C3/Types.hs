{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module C3.Types where

import Data.Aeson
import Data.Default
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Vector (fromList)
import GHCJS.Types (JSVal)

import C3.Chart.Gauge

-- | A reference to the chart object returned by C3.
newtype Chart = Chart { getChart :: JSVal }
newtype DataIndex = DataIndex { ids :: Text } deriving ToJSON

mField :: (ToJSON a, Functor f) => Text -> f a -> f (Text, Value)
mField field = fmap ((field .=) . toJSON)

infixr 6 .=?
(.=?) :: (ToJSON a, Functor f) => Text -> f a -> f (Text, Value)
(.=?) = mField

-- | The definition of the chart we want to generate.
data ChartOptions = ChartOptions
  { -- | The CSS selector the chart will be set to.
    bindTo    :: Text
    -- | The data sourcing the chart.
  , chartData :: ChartData
  , chartSizeOptions :: Maybe ChartSizeOptions
  }

instance Default ChartSizeOptions where
  def = ChartSizeOptions Nothing Nothing

data ChartSizeOptions
  = ChartSizeOptions
  { chartSizeOptionsHeight :: Maybe Int
  , chartSizeOptionsWidth :: Maybe Int
  }

-- | The data source for our chart.
data ChartData = ChartData
  { -- | The chart type.
    chartType    :: ChartType
    -- | The columns of data.
  , chartDatum :: Datum
  }

data Datum
  = Columns [Column]
  -- | Json Text
  -- | Row [Text] [[Double]]
  -- | Url Text

instance ToJSON Datum where
  toJSON (Columns columns) = object [ "columns" .= map toJSON columns ]

instance ToJSON Column where
  toJSON Column{..} = Array (fromList (String columnName : map toJSON columnValues))

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
  toJSON ChartOptions{..}
    | Gauge opts <- chartType chartData = object (base ++ ["gauge" .= opts])
    | otherwise = object base
   where
     base = [
         "bindto" .= bindTo
       , "data"   .= chartData ] <> msize
     msize = case chartSizeOptions of
       Nothing -> mempty
       Just sizeOpts -> [ "size" .= toJSON sizeOpts ]

instance ToJSON ChartSizeOptions where
  toJSON ChartSizeOptions{..} = object $ catMaybes [
      "height" .=? chartSizeOptionsHeight
    , "width"  .=? chartSizeOptionsWidth ]

instance ToJSON ChartData where
  toJSON ChartData{..} = object conjoined
    where
      base = [ "type"    .= toJSON chartType ]
      conjoined =  base ++ datum
      datum = case chartDatum of
        Columns c -> ["columns" .= toJSON c]

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
  toJSON (Gauge _)  = String "gauge"
