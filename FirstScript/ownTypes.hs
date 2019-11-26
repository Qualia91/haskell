-- Define type using data keyword.
data Couple = One | Two | Many deriving (Show, Read, Eq)
-- Above is an example of using verticle bars to define
-- alternatives (like enumerates?).
-- Deriving show makes it possible to print object to window.
-- Deriving read makes it pissibke to convert strings to this type
-- ie read "One" :: Couple
-- Deriving Eq makes it possible to use comparison stuff
-- ie One == One

-- type constructure that takes string, int and int and returns
-- Score
data CricketScore = Score [Char] Int Int deriving Show