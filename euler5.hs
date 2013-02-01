euler5 :: Int
euler5 = powersOf(countMaxExponent(allPrimeFactorsBetween 2 20))
    where
    allPrimeFactorsBetween :: Int -> Int -> [[Int]]
    allPrimeFactorsBetween min max 
        | min < max = primeFactors(min):allPrimeFactorsBetween (min+1) max
        | min == max = [primeFactors(min)]
        where
            primeFactors :: Int -> [Int]
            primeFactors n
                | null firstPrime = [n]
                | otherwise = head firstPrime:primeFactors (n `div` head firstPrime)
                where
                    firstPrime = [x|x<-[2..n `div` 2], n `mod` x == 0]

    countMaxExponent :: [[Int]] -> [(Int, Int)]
    countMaxExponent xs = countMaxExponentFor xs [2..20]
        where 
            countMaxExponentFor :: [[Int]] -> [Int] -> [(Int, Int)]
            countMaxExponentFor xs [y] = [tupleWithExpAndCount xs y] 
            countMaxExponentFor xs (y:ys) = tupleWithExpAndCount xs y:countMaxExponentFor xs ys

            tupleWithExpAndCount :: [[Int]] -> Int -> (Int, Int)
            tupleWithExpAndCount xs y = (y, maximum(map (countItem y) xs))

            countItem :: Int -> [Int] -> Int
            countItem n xs = length (filter (== n) xs)

    powersOf :: [(Int,Int)] -> Int
    powersOf [] = 0
    powersOf xs = foldr(*) 1 (map(powerOf) xs)
        where 
            powerOf :: (Int,Int) -> Int
            powerOf (x,y) = x^y
