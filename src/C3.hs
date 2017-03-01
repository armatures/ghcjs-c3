module C3 
  ( module X
  , generate
  , transform
  , loadData
  ) where

import GHCJS.Marshal (toJSVal_aeson)

import C3.Foreign as X
import C3.Types   as X

-- | Generate a chart using C3.
generate :: ChartOptions -> IO Chart
generate = (=<<) js_c3_generate . toJSVal_aeson

loadData :: Chart -> ChartData -> IO Chart
loadData chart column_data = toJSVal_aeson column_data >>= js_c3_load chart

-- | Alter the chart type of a previously rendered chart.
transform :: Chart -> ChartType -> IO ()
transform chart chartTyp = toJSVal_aeson chartTyp >>= js_c3_transform chart
