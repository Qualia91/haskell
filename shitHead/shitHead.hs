
import Data.List
import Text.Read

main = do
	putStrLn "Enter number of players"
	numOfPlayers <- getLine
	case readMaybe numOfPlayers :: Maybe Int of
		Just x <= 1 ->
			putStrLn "You need to have more than one player" >> main

		Just x -> do
			print (lengthOfList (getCards x))
			putStrLn ("Created dec")
			case runGame (getCards x) of
				True -> main
			
		Nothing ->
			putStrLn "You didnt enter a valid number of players" >> main

getCards :: Int -> [Int]
getCards x
	| x <= 1 = []
	| otherwise = take (52 * numberOfDecs) (cycle [2..14])
	where 
		numberOfDecs = (div (x - 1) 4) + 1

runGame :: [Int] -> Bool
runGame dec = True

lengthOfList :: (Num b) => [a] -> b
lengthOfList [] = 0
lengthOfList xs = sum[1 | _ <- xs]