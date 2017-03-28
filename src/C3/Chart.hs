module C3.Chart where

import Data.Text (Text)
import C3.Types

fromKeyValue :: [(Text, Double)] -> Datum
fromKeyValue = Columns . map (\(k,v) -> Column k [v])
