
lst_ = map f lst

g = (+)

lst__ = foldl g 1 lst

f x = x*(x+1)

lst = [1..10]

main = do {
	print lst_;
	print lst__;
}
