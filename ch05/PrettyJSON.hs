module PrettyJSON (renderJValue) where

import Numeric (showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)

renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue  JNull        = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary)  = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where field (name, val)  = string name <> text ": " <> renderJValue val

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item =
  enclose open close . fsep . punctuate (char ',') . map item
  where enclose left right x = char left <> x <> char right

string :: String -> Doc
string = text . concatMap oneChar
  where oneChar :: Char -> String
        oneChar c = case lookup c simpleEscapes of
                      Just r -> r
                      Nothing
                        | mustEscape c -> hexEscape c
                        | otherwise    -> c:""
            where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

                  simpleEscapes :: [(Char, String)]
                  simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
                    where ch a b = (a, ['\\',b])

                  hexEscape :: Char -> String
                  hexEscape c | d < 0x10000 = smallHex d
                              | otherwise   = astral (d - 0x10000)
                    where d = ord c

                          astral :: Int -> String
                          astral n = smallHex (a + 0xd800)
                                  ++ smallHex (b + 0xdc00)
                            where a = (n `shiftR` 10) .&. 0x3ff
                                  b = n .&. 0x3ff

                          smallHex :: Int -> String
                          smallHex x = "\\u"
                                    ++ (replicate (4 - length h) '0')
                                    ++ h
                            where h = showHex x ""
