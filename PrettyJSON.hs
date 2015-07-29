module PrettyJSON 
  ( prettyJSON 
  ) where

import SimpleJSON (JValue(..))
import Prettify (Doc, string, double, text, series, (<>), hcat)

prettyJSON :: JValue -> Doc
prettyJSON (JString   s) = string s
prettyJSON (JNumber   n) = double n
prettyJSON (JBool  True) = text "true"
prettyJSON (JBool False) = text "false"
prettyJSON JNull         = text "null"
prettyJSON (JObject   o) = series '{' '}' field o
   where field (k, v) = string k <> text ": " <> prettyJSON v  
prettyJSON (JArray    a) = series '[' ']' prettyJSON a


