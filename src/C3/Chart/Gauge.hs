{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module C3.Chart.Gauge where

import Data.Aeson
import Data.Default
import Data.Text

data GaugeOpts = GaugeOpts
  { gaugeExpand :: Bool
  , gaugeMin    :: Int
  , gaugeMax    :: Int
  , gaugeUnits  :: Maybe Text
  , gaugeWidth  :: Maybe Int
  } deriving (Show)

instance Default GaugeOpts where
  def = GaugeOpts True 0 100 Nothing Nothing

instance ToJSON GaugeOpts where
  toJSON GaugeOpts{..} = object [
    "expand" .= gaugeExpand,
    "min"    .= gaugeMin,
    "max"    .= gaugeMax,
    "units"  .= gaugeUnits,
    "width"  .= maybe (String "auto") (Number . fromIntegral) gaugeWidth ]
