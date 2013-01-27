reduce :: Int -> Int
reduce n = if null(facts)
            then n 
            else reduce (n `div` head facts)
           where 
            facts = take 1 [x|x <- [2.. n `div` 2], n `mod` x == 0]

euler3 :: Int
euler3 = reduce 600851475143
