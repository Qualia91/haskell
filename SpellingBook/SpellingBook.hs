-- The function used by foldr
-- inputs the element and accumulator, both as [Char],
-- and outputs a [char] of the accumulated phrase
appendElements :: [Char] -> [Char] -> [Char]
-- using pattern matching to catch an empty element for safety
appendElements "" acc = "" ++ acc
-- pattern matching to impliment the "and"
-- in the statement for the last element, ie when accumulator is ""
appendElements elem "" = "and " ++ [(elem!!0)] ++ " is for " ++ elem ++ "!"
-- Because i am finding the last element by whether the accumulator is empty,
-- i also have a patteren matched case for when the list has only got one
-- element in, so singular lists wont have and in the phrase
appendElements elem "!" = [(elem!!0)] ++ " is for " ++ elem ++ "!"
appendElements elem acc = [(elem!!0)] ++ " is for " ++ elem ++ ", " ++ acc
-- As elem is a [Char], elem!!0 is the first letter. However this is
-- of type Char, and so needs to go back into brackets to make it a 
-- [Char]

-- used by the filtering method to get rid of empty elements
-- First removes all whitespace in word, then returns true if yes. 
isWordEmpy :: [Char] -> Bool
isWordEmpy word = (filter (/=' ') word) /= ""

speller :: [[Char]] -> [Char]
-- initial function to start the program
-- Takes in a list of words and returns a string containing
-- what the speller would say in a childrens book
-- Using pattern matching to catch an empty list
speller [] = "No words entered"
speller listOfWords = do
	-- using fold r to start with the last element to get that pesky
	-- "and" in the sentance
	-- A filter is used to remove empty elements
	let filteredList = filter isWordEmpy listOfWords;
	-- Then an if statement is used to supplie an accumulator to the appendElements function
	foldr (appendElements) (if length filteredList == 1 then "!" else "") filteredList
