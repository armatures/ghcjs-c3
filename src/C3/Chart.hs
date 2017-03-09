module C3.Chart where

import Data.Text (Text)
import C3.Types

fromKeyValue :: [(Text, Double)] -> Datum
fromKeyValue kv = Rows headers values
  where
    values = map (\a -> [snd a]) kv
    headers = map fst kv
