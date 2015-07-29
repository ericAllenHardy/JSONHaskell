module SimpleJSON 
  ( JValue(..)
  , getString
  , getNumber
  , getInt
  , getBool
  , getObject
  , getArray
  , isNull
  ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

--getVal :: (a-> JValue) -> Jvalue -> Maybe a

getString (JString str) = Just str
getString _             = Nothing

getNumber (JNumber d) = Just d
getNumber _           = Nothing

getInt (JNumber d) = Just $ truncate d
getInt _           = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArrayy _          = Nothing

isNull = (== JNull)



