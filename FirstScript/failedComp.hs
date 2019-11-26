-- Maybe
-- like optional
maxHelper :: Int -> [Int] -> Int
maxHelper x [] = x
maxHelper x (y:ys) = maxHelper (if x>y then x else y) ys

maxFromList :: [Int] -> Maybe Int
maxFromList [] = Nothing
maxFromList (x:xs) = Just (maxHelper x xs)

-- propogate maybe (Monads)
-- use fmap before function call where you want to se maybe
