module XmlParser (parseShow) where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List

parseShow :: String -> String
parseShow = run_parser showParser

run_parser :: Parser a -> String -> a
run_parser p str = case parse p "" str of
	-- The parse function must return an either as Left and Right
	-- are either outputs. Left contains error, Right contains answer
	Left err -> error $ "parse error at " ++ (show err)
	Right val -> val

-- this is the one that finds the correct parser by using the try
-- things <|>
showParser :: Parser String
showParser =
    list_parser <|> -- [ ... ]
    tuple_parser <|> -- ( ... )
    try record_parser <|> -- MkRec { ... }
    adt_parser <|> -- MkADT ...
    number <|>    -- signed integer
    quoted_string <?> "Parse error"


joinNL :: [String] -> String -- join lines using "\n"
joinNL ls = intercalate "\n" ls

-- xml specific data structures below
-- Header
xml_header =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- tags
otag t = "<"++t++">"
ctag t = "</"++t++">"
tag t v = concat [otag t,v,ctag t]

-- Attrubutes
tagAttrs :: String -> [(String,String)] -> String -> String 
tagAttrs t attrs v = 
    concat [
        otag (unwords $ [t]++(map (\(k,v) -> concat [k,"=\"",v,"\""]) attrs))
        ,v
        ,ctag t
        ]

-- basic form of each:
--		You get the information from the function this is passed to
--		via the <- and return the xml string that corrisponds to it
-- lists
list_parser = do
    ls <- brackets $ commaSep showParser 
    return $ tag "list" $ joinNL $ map (tag "list-elt") ls

-- tuples
tuple_parser = do
    ls <- parens $ commaSep showParser 
    return $ tag "tuple" $ unwords $ map (tag "tuple-elt") ls

-- record
record_parser = do
    ti <- type_identifier
    ls <- braces $ commaSep kvparser
    return $ tagAttrs "record" [("name",ti)] (joinNL ls)

-- key value pair
kvparser = do
    k <- identifier
    symbol "="
    t <- showParser
    return $ tagAttrs "elt" [("key",k)] t
    
-- type
type_identifier = do
    fst <- oneOf ['A' .. 'Z']
    rest <- many alphaNum
    whiteSpace
    return $ fst:rest  

-- Algebraic data types
adt_parser = do
    ti <- type_identifier 
    return $ tag "adt" ti

-- quoted string
quoted_string = do
    s <- stringLiteral
    return $ "\""++s++"\""

-- number
number = do
    n <- integer
    return $ show n

-- Functions used above from preluse
-- concat :: [[a]] -> [a] -- join lists
-- unwords :: [String] -> String -- join words using spaces


-- shorter names for prediefined parsers in the P library
lexer = P.makeTokenParser emptyDef
parens = P.parens lexer
brackets = P.brackets lexer
braces = P.braces lexer
commaSep = P.commaSep lexer
whiteSpace = P.whiteSpace lexer
symbol = P.symbol lexer
identifier = P.identifier lexer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer