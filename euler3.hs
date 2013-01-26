getFirstPrimeFactor :: Int -> Int
getFirstPrimeFactor x = sieves(factors(x) x)
	where
		factors :: Int -> [Int]
		factors n = [x|x <- [floor(sqrt(fromIntegral(n)))..2], n `mod` x == 0]
		
		sieves :: Int -> [Int] -> Int
		sieves n [] = 0
		sieves n [x] = x
		sieves n (x:xs) =	if length(withoutFactor) == length(xs)
						then x
						else sieves n/x withoutFactor
					where
						withoutFactor = filter (`mod` x == 0) xs


euler3 :: [Int]
euler3 = getFirstPrimeFactor 600851475143
