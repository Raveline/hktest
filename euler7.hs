euler7 :: Int
euler7 = last . take 10001 $ sieve 
    where
        sieve :: [Int]
        sieve = sieve' [2..]
        sieve' :: [Int] -> [Int]
        sieve' (x:xs) = x : sieve' [n|n <- xs, n `mod` x > 0]

