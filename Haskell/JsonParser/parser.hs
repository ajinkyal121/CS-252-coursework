import Text.ParserCombinators.Parsec
import System.Environment

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
  deriving (Eq, Ord, Show)

jsonFile :: GenParser Char st JValue
jsonFile = do
  result <- jsonElem
  spaces
  eof
  return result

jsonElem :: GenParser Char st JValue
jsonElem = do
  spaces
  result <- jsonElem'
  spaces
  return result

jsonElem' = jsonArr
        <|> jsonString
        <|> jsonNumber
        <|> jsonObject
        <|> jsonBool
        <|> jsonNull
 
jsonString :: GenParser Char st JValue
jsonString = jsonStringDQ <|> jsonStringSQ

jsonStringDQ = do
  char '"'
  s <- many $ noneOf "\"" -- crude.  does not allow double quotes within strings
  char '"'
  return $ JString s

jsonStringSQ = do
  char '\''
  s <- many $ noneOf "'" -- crude, same as above
  char '\''
  return $ JString s

jsonBool = do
  bStr <- string "true" <|> string "false"
  return $ case bStr of
    "true" -> JBool True
    "false" -> JBool False

jsonNull = do
  string "null"
  return JNull

jsonArr = do
  char '['
  arr <- jsonElem `sepBy` (char ',')
  char ']'
  return $ JArray arr

jsonObject = do
  char '{'
  obj <- jsonObject' `sepBy` (char ',')
  char '}'
  return $ JObject obj

jsonObject' = do
  skipMany1 space -- Skip spaces
  key <- many $ noneOf ":"
  char ':' -- Just a seperator
  value <- jsonElem
  return $ (key, value)
  
jsonNumber = do
  jnum <- many1 digit
  return $ JNumber (read jnum)


prettyPrint :: JValue -> IO ()
prettyPrint jr = putStrLn $ beautify 0 jr

indent :: Int -> String 
indent i = take i (repeat ' ')

beautify :: Int -> JValue -> String 
beautify i jr = indent i ++ beautify' i jr


beautify' :: Int -> JValue -> String
beautify' _ JNull= "null"
beautify' _ (JNumber n) = show n
beautify' _ (JBool True) = "true"
beautify' _ (JBool False) = "false"
beautify' _ (JString str) = "'" ++ str ++ "'"
beautify' i (JArray xs) = "[\n" ++ showArrayElems xs (i+2) ++ "\n" ++ indent i ++ "]"
beautify' i (JObject obj) = "{\n" ++ showObjKeyVals obj (i+2) ++ "\n" ++ indent i ++ "}"

showArrayElems elems i = 
  let strList = map (beautify i) elems; 
        fn = (\elem rest ->  if rest == "" then elem else elem ++ ",\n" ++ rest)
  in foldr fn "" strList

showObjKeyVals [] _ = ""
showObjKeyVals ((key,jv): xs) i = 
  indent i ++ "'" ++ key ++ "': " ++ beautify' i jv
    ++ if null xs then "" else ",\n" ++ showObjKeyVals xs i


parseJSON :: String -> Either ParseError JValue
parseJSON input = parse jsonFile "(unknown)" input

main = do
  args <- getArgs
  p <- parseFromFile jsonFile (head args)
  case p of
    Left err  -> print err
    Right json -> prettyPrint json
