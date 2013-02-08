euler6 :: Int
euler6 = (squareOfSumUpTo 100) - (sumOfSquaresUpTo 100)
    where
        sumOfSquaresUpTo :: Int -> Int
        sumOfSquaresUpTo n = sum[x^2|x <- [1..n]]

        squareOfSumUpTo :: Int -> Int
        squareOfSumUpTo n = sum[1..n]^2
