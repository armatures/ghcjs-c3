{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module C3.Types where

import Data.Aeson
import Control.Lens (makeLenses)
import Data.Maybe (catMaybes)
import Data.Default
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
    _bindTo    :: Text
    -- | The data sourcing the chart.
  , _chartData :: ChartData
  , _chartSizeOptions :: Maybe ChartSizeOptions
  }

instance Default ChartOptions where
  def = ChartOptions "my-chart" def def

data ChartSizeOptions
  = ChartSizeOptions
  { _chartHeight :: Maybe Int
  , _chartWidth :: Maybe Int
  }

instance Default ChartSizeOptions where
  def = ChartSizeOptions Nothing Nothing

-- | The data source for our chart.
data ChartData = ChartData
  { -- | The chart type.
    _chartType    :: ChartType
    -- | The columns of data.
  , _chartDatum :: Datum
  , _chartOptionalData :: Maybe OptionalChartData
  }


instance Default ChartData where
  def = ChartData Bar (Columns []) Nothing

-- XXX  attach this to a column definition somehow to avoid errors
type DatumIndex = Text

data Color
  = Color
  { datumIndex :: DatumIndex
  , colorHex :: Text
  }

newtype Colors = Colors { unColors :: [Color] }
newtype Group = Group { unGroup :: [Text] }

instance ToJSON Group where
  toJSON (Group gs) = toJSON gs

data OptionalChartData
  = OptionalChartData
  { _chartGroups :: Maybe [Group]
  , _chartColors :: Maybe Colors
  }

instance Default OptionalChartData where
  def = OptionalChartData
    { _chartGroups = Just $ [ Group [] ]
    , _chartColors = Nothing
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
    | Gauge opts <- _chartType _chartData = object (base ++ ["gauge" .= opts])
    | otherwise = object base
   where
     base = [
         "bindto" .= ("#" <> _bindTo)
       , "data"   .= _chartData ] <> msize
     msize = case _chartSizeOptions of
       Nothing -> mempty
       Just sizeOpts -> [ "size" .= toJSON sizeOpts ]

instance ToJSON ChartSizeOptions where
  toJSON ChartSizeOptions{..} = object $ catMaybes [
      "height" .=? _chartHeight
    , "width"  .=? _chartWidth ]

instance ToJSON ChartData where
  toJSON ChartData{..} = object conjoined
    where
      base = [ "type"    .= toJSON _chartType ]
      conjoined =  base ++ datum ++ optional
      datum = case _chartDatum of
        Columns c -> ["columns" .= toJSON c]
      optional = catMaybes [ "groups" .=? (fmap (toJSON . _chartGroups) _chartOptionalData)
        , "colors" .=? (fmap (toJSON . _chartColors) _chartOptionalData) ]

instance ToJSON OptionalChartData where
  toJSON OptionalChartData{..} = object $ catMaybes [
      "groups" .=? _chartGroups
    , "colors" .=? _chartColors ]

instance ToJSON Colors where
  toJSON (Colors colors) = object $ fmap m colors
    where m (Color i c) = (i .= c)

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

makeLenses ''ChartOptions
makeLenses ''OptionalChartData
makeLenses ''ChartData
