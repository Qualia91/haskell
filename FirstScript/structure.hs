-- let expression
journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre = 
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles/milespergallon
 in (gallons*litrespergallon*fuelcostperlitre)

 -- the let defines variables, the stuff after in is the 
 -- return of the function

 -- Where clause
squareplusone :: Int -> Int
squareplusone x = xsquared + 1
 where xsquared = x*x

-- ike opposite way round to let


-- Guards: functions based on predicate values
absolute :: Int -> Int
absolute x
	| x < 0 = -x
	| otherwise = x
-- Otherwise is catch as every function needs to return something

-- a where caluse at the end of guards will define stuff for all
-- outcomes of guard


-- Case statements
data Pet = Cat | Dog | Fish | Parrot String | Human
hello :: Pet -> String
hello x =
	case x of
		Cat -> "Meeow"
		Dog -> "Woof"
		Fish -> "Bubble"
		Parrot name -> "Pretty " ++ name
		_ -> "Grunt"
-- Case statment to do different things based on type
-- Can even input variable with it, like the parrot case
-- This object construction is defined at the data Pet line
-- _ means all others, its an otherwise statement