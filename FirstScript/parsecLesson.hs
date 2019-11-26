module Main where
import XmlParser (parseShow)

data PersonRecord = MkPersonRecord {
	name :: String,
	address :: Address,
	id :: Integer,
	labels :: [Label]
} deriving (Show)

data Address = MkAddress {
	line1 :: String,
	number :: Integer,
	street :: String,
	town :: String,
	postcode :: String
} deriving (Show)

data Label = Green | Red | Blue | Yellow deriving (Show)

rec1 = MkPersonRecord
	"Name1"
	(MkAddress "School" 17 "Street" "Town" "Postcode")
	1234
	[Green, Red]

rec2 = MkPersonRecord
	"Name2"
	(MkAddress "School1" 18 "Street1" "Town1" "Postcode1")
	5678
	[Blue, Yellow]

-- main = putStrLn $ show [rec1, rec2]

rec_str =  show [rec1,rec2]    
main = putStrLn $ parseShow rec_str