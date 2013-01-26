getFirstPrimeFactor :: Int -> Int
getFirstPrimeFactor x = last(sieves x (factors x))
    where
        factors :: Int -> [Int]
        factors n = [x|x <- [2.. firstPotentialFactor], n `mod` x == 0]
            where firstPotentialFactor = n `div` 2

        sieves :: Int -> [Int] -> [Int]
        sieves n [] = []
        sieves n [x] = [x]
        sieves n (x:xs) = x:sieves n (filter(isNotFactorOf x) xs)
            where
                isNotFactorOf :: Int -> Int -> Bool
                isNotFactorOf f n = n `mod` f /= 0

euler3 :: Int
euler3 = getFirstPrimeFactor 600851475143
