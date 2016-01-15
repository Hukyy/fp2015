listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber xs = foldl iter 0 xs
	where
		iter m p = 10*m + p

suffix :: (Eq a) => [a] -> [a] -> Bool
suffix xs ys = a == b
	where
		a = reverse xs
		b = take (length xs) $ reverse ys

occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences (x:xs) ys =  (length $ filter (==x) ys) : occurrences xs ys

removeAt :: Int -> [a] -> [a]
removeAt n lst
	|n<0 = error "ne moje"
	|n>= length lst = error "pak ne moje"
	|otherwise = take n lst ++ drop (n+1) lst
