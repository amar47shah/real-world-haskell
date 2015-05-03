module SimpleJSON
  ( JValue(..)
  , getArray
  , getBool
  , getDouble
  , getInt
  , getObject
  , getString
  , isNull
  ) where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
              deriving (Eq, Ord, Show)

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

isNull :: JValue -> Bool
isNull = (== JNull)
