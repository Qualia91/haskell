masterMind :: String -> String -> Int -> IO ()
-- main function. inputs string of all colours available,
-- number of guesses you get and list of colours your opponent
-- picks as an answer. The colours are one letter
masterMind allColours listToGuess numberOfGuesses = putStrLn (turn allColours listToGuess ['-' | x <- listToGuess] numberOfGuesses)

turn :: String -> String -> String -> Int -> String
turn allColours listToGuess guessedList numberOfGuesses = do {
	if numberOfGuesses == 0
		then "You Lose";
		else if listToGuess==guessedList
			then "You win";
			else makeGuess allColours listToGuess guessedList numberOfGuesses;
}

makeGuess :: String -> String -> String -> Int -> String
makeGuess allColours listToGuess guessedList numberOfGuesses = 
 do putStrLn (guessedList ++ " " ++ take numberOfGuesses (repeat '*'))
    putStr " Enter your guess: "
    q<-getLine
    let guessedList' = updateString listToGuess guessedList q
    let n' = if correct then numberOfGuesses else numberOfGuesses-1
    turn allColours listToGuess guessedList' n'


updateString :: String -> String -> String -> (Bool, String)
updateString listToGuess guessedList newGuess = [if x == y then x else "-" | (x, y) <- zip listToGuess guessedList]
