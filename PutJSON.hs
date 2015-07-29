module PutJSON 
  (renderJValue
  ) where
import SimpleJSON
import Data.List (intercalate)

renderJValue :: JValue -> String
renderJValue (JString   s) = show s
renderJValue (JNumber   n) = show n
renderJValue (JBool  True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject   o) = "{" ++ pairs o ++ "}"
  where pairs = (intercalate ", ") . (map renderPair)
        renderPair (k, v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray    a) = "[" ++ values a ++ "]"
  where values = (intercalate ", ") . (map renderJValue)

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue
