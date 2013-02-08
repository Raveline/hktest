import qualified Data.Map as Map 

euler5 :: Int
euler5 = powersOf(countMaxExponent(allPrimeFactorsTo 20))
    where
    allPrimeFactorsTo :: Int -> [[Int]]
    allPrimeFactorsTo max = [primeFactors(x)|x <- [2..max]]
        where
            primeFactors :: Int -> [Int]
            primeFactors n
                | null firstPrime = [n]
                | otherwise = head firstPrime:primeFactors (n `div` head firstPrime)
                where
                    firstPrime = [x|x<-[2..n `div` 2], n `mod` x == 0]

countMaxExponent :: [[Int]] -> Map.Map Int Int
countMaxExponent xs = Map.fromList(zip [1..20] [maximum(map(countItem y) x)|x <- [xs], y <- [1..20]])
    where
        countItem :: Int -> [Int] -> Int
        countItem y xs = length(filter(== y) xs)
           
powersOf :: Map.Map Int Int -> Int
powersOf xs = Map.foldWithKey f 1 xs
    where
        f :: Int -> Int -> Int -> Int
        f k v r = r*(k^v)
