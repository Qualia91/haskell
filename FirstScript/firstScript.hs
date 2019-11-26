doubleMe x = x + x
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]

fact :: Int -> Int
fact 
fact 0 = 1
fact n = n*fact(n-1)

-- list recursion
-- length explantion
-- (x:xs) = x cons xs
lengthExpl :: [a] -> Int
lengthExpl [] = 0 -- base case
lengthExpl (x:xs) = 1 + lengthExpl xs -- uses recurion untill xs fits in base case

-- function composition
-- chaining functions together uses the . function
(.) :: (b->c) -> (a->b) -> a -> c
(f.g) x = f (g x)

-- using above
-- func1 = \x -> x + 1
-- func2 = \x -> x + 1
-- (func1.func1) 2 will equal 4 as func1 and func2 are chained