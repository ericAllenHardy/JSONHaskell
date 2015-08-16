module JSONParse 
  ( readJSON
  ) where

import Text.ParserCombinators.Parsec
import Numeric (readFloat)

import SimpleJSON(JValue(..), jary, jobj)

readJSON :: String -> Maybe JValue
readJSON s = case parse parseJSON "" s of
               Left  _ -> Nothing
               Right x -> Just x

parseJSON = do x <- parseString <|> try parseNum
                <|> parseBool   <|> parseNull
                <|> parseArray  <|> parseObject
               eof
               return x
               


safeString :: (String -> a) -> Parser a
safeString f = do char '"'
                  s <- many itsAChar
                  char '"'
                  return $ f s
  where itsAChar = noneOf "\""
               <|> do char '\\'
                      c <- oneOf "\\\"'ntr"
                      return $ case c of
                        'n' -> '\n'
                        'r' -> '\r'
                        't' -> '\t'
                        _   -> c

parseString :: Parser JValue
parseString = safeString JString

parseNum :: Parser JValue
parseNum = parseInt <|> parseFloat

parseInt :: Parser JValue
parseInt = do d <- many1 digit
              return $ JNumber $ read d

parseFloat :: Parser JValue
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ JNumber $ fst . head $ readFloat (x ++ "." ++ y)

parseBool :: Parser JValue
parseBool  = parseTrue <|> parseFalse
parseTrue  = string "\"true\""  >> (return $ JBool True)
parseFalse = string "\"false\"" >> (return $ JBool False)
               
parseNull :: Parser JValue
parseNull = string "null" >> return JNull

parseObject :: Parser JValue
parseObject = do char '{'
                 obj <- sepBy parseObj $ do char ','
                                            optional space
                 char '}'
                 return $ JObject $ jobj obj
  where parseObj = do key <- safeString id 
                      char ':'
                      optional space
                      val <- parseJSON
                      return $ (key, val)

parseArray :: Parser JValue
parseArray = do char '['
                xs <- sepBy parseJSON $ do char ','
                                           optional space
                char ']'
                return $ JArray $ jary xs
