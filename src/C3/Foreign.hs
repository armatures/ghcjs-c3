{-# LANGUAGE OverloadedStrings #-}
module C3.Foreign where

import Data.JSString (JSString)
import GHCJS.Types   (JSVal)
import C3.Types

foreign import javascript unsafe
  "$r = c3.generate($1);" 
  js_c3_generate :: JSVal -> IO Chart

foreign import javascript unsafe
  "$1.transform($2);"
  js_c3_transform :: Chart -> JSVal -> IO ()

foreign import javascript unsafe
  "function createChartContainer(bindto) {\
  \  var e = document.createElement('div');\
  \  e.id = bindto;\
  \  document.body.appendChild(e);\
  \}createChartContainer($1);"
  js_createChartContainer :: JSString -> IO ()

