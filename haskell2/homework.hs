isPrime :: Int -> Bool
isPrime n
    | n <= 1 = False
    | otherwise = isPrime' 2 n
        where
            isPrime' current n
                | current == n = True
                | mod n current == 0 = False
                | otherwise = isPrime' (current + 1) n

truncatablePrime::Int->Bool
truncatablePrime n
	|n<=10 && isPrime n = True
	|otherwise = isPrime n && truncatablePrime(mod n 10)

		
containsDigits::Int->Int->Bool
containsDigits num1 num2
	| num2==0 = True
	| contains num1 (mod num2 10) = containsDigits num1 (div num2 10)
	| otherwise=False
		where
			contains x y
				| x==0 && y/=0 = False
				| x/=y = False || contains (div x 10) y
				| mod x 10 == y = True



productOfDigits::Int->Int
productOfDigits 0 = 1
productOfDigits n = (mod n 10)* productOfDigits (div n 10)

sumdiv::Int->Int
sumdiv n = foldr (+) 0 [x | x<- [1..(n-1)],mod n x == 0]

interestingNumber::Int->Bool
interestingNumber n = n==sumdiv (sumdiv n)

quadrant :: Double -> Double -> Int
quadrant x y
	|x==0 && y==0 = 0
	|x>0 && y>0 = 1
	|x<0 && y>0 = 2
	|x<0 && y<0 = 3
	|x>0 && y<0 = 4
