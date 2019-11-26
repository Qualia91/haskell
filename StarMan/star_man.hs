
updateString :: String -> String -> Char -> (Bool, String)
updateString secretWord currentWord guessedChar 
 = (elem guessedChar secretWord, [if x==guessedChar 
 then guessedChar 
 else y | (x, y) <- zip secretWord currentWord])

turnFunc :: String -> String -> Int -> IO ()
turnFunc secretWord currentWord numberOfTurns = do {
 if numberOfTurns==0
   then putStrLn "You Lose";
   else if secretWord==currentWord;
    then putStrLn "You Win!";
    else makeGuess secretWord currentWord numberOfTurns;
}

makeGuess :: String -> String -> Int -> IO ()
makeGuess secretWord currentWord n = 
 do putStrLn (currentWord ++ " " ++ take n (repeat '*'))
    putStr " Enter your guess: "
    q<-getLine
    let (correct, currentWord') = updateString secretWord currentWord (q!!0)
    let n' = if correct then n else n-1
    turnFunc secretWord currentWord' n'


starMan :: String -> Int -> IO ()
starMan secretWord n = turnFunc secretWord ['-' | x <- secretWord] n