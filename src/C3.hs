module C3 
  ( module X
  , generate
  , transform
  , load
  ) where

import GHCJS.Marshal (toJSVal_aeson)

import C3.Foreign as X
import C3.Types   as X

-- | Generate a chart using C3.
generate :: ChartOptions -> IO Chart
generate = (=<<) js_c3_generate . toJSVal_aeson

-- | Load new data into an existing chart
load :: Chart -> Datum -> IO Chart
load chart datum = toJSVal_aeson datum >>= js_c3_load chart

-- | Unload a specific data set from an existing chart
-- unloadData :: Chart -> DataIndex -> IO Chart
-- unloadData chart index = toJSVal_aeson index >>= js_c3_unload chart

-- | Alter the chart type of a previously rendered chart.
transform :: Chart -> ChartType -> IO ()
transform chart chartTyp = toJSVal_aeson chartTyp >>= js_c3_transform chart
