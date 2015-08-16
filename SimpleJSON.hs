{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module SimpleJSON 
  ( JValue(..)
  , JAry(fromJAry), jary
  , JObj(fromJObj), jobj
  , JSONData(..)
  , isNull
  ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray  (JAry JValue)
              deriving (Eq, Ord, Show)

newtype JAry a = JAry {
    fromJAry :: [a]
  } deriving (Eq, Ord, Show)
jary = JAry

newtype JObj a = JObj {
    fromJObj :: [(String, a)]
  } deriving (Eq, Ord, Show)
jobj = JObj

instance Functor JObj where
  fmap f = JObj . map (\(k, v) -> (k, f v)) . fromJObj

instance Functor JAry where
  fmap f = JAry . (map f) . fromJAry


class JSONData a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Maybe a 

instance JSONData JValue where
  toJValue = id
  fromJValue = Just

instance JSONData String where
  toJValue = JString

  fromJValue (JString str) = Just str
  fromJValue _             = Nothing

instance JSONData Double where
  toJValue = JNumber

  fromJValue (JNumber d) = Just d
  fromJValue _           = Nothing

instance JSONData Int where
  toJValue = JNumber . fromIntegral

  fromJValue (JNumber d) = Just $ truncate d
  fromJValue _           = Nothing

instance JSONData Bool where
  toJValue = JBool

  fromJValue (JBool b) = Just b
  fromJValue _         = Nothing

instance (JSONData a) => JSONData (JObj a) where
  toJValue = JObject . (fmap toJValue) 

  fromJValue (JObject (JObj o)) = fmap JObj $ mapM keyConvert o
    where keyConvert (k, x) = case fromJValue x of
                               Nothing -> Nothing
                               Just y  -> Just (k, y)
  fromJValue _ = Nothing

instance (JSONData a) => JSONData (JAry a) where
  toJValue = JArray . (fmap toJValue)

  fromJValue (JArray (JAry a)) = fmap JAry $ mapM fromJValue a
  fromJValue _                 = Nothing

isNull = (== JNull)
