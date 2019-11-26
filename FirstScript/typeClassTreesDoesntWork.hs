-- Tree is type constructor, a is the polymorphic type variable
-- effectively making it possible to put any type in
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving Show

treeDepth :: Num a => Tree a -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
	1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

treeSum :: Num a => Tree a -> a
treeSum Leaf = 0
treeSum (Node value leftSubtree rightSubtree) = 
	value + (treeDepth leftSubtree) + (treeDepth rightSubtree)

isSortedTree :: Num a => Tree a -> a -> a -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted   = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

addNewMax :: Num a => Tree a -> Tree a
-- add new max element
-- base behaviour for finding a leaf
addNewMax Leaf = Node 0 Leaf Leaf
-- add new max val on
addNewMax (Node val left Leaf) = Node val left (Node (val+1) Leaf Leaf)
-- find the rightmost node
addNewMax (Node val left right) =  Node val left (addNewMax right)
-- has to be in this order or specific case gets overlooked by general case
-- ie an input of Node val left Leaf would fit in Node val left right if
-- that was before

-- add value to tree in order
addValueInOrder :: Num a => Tree a -> a -> Tree a
-- base behaviour for leaf found
addValueInOrder Leaf newVal = Node newVal Leaf Leaf
-- find its position and add value
addValueInOrder (Node val left right) newVal =
	if newVal >= val then
		Node val left (addValueInOrder right newVal)
		else Node val (addValueInOrder left newVal) right
	
-- convert tree to list in order if filed correctly (binary tree)
convertTreeToList :: Num a => Tree a -> [a]
-- base value for leaf
convertTreeToList Leaf = []
-- Traverse tree and append to a list as you go
convertTreeToList (Node val left right) = 
	(convertTreeToList left) ++ [val] ++ (convertTreeToList right)