import Data.List

listFactorsBetween :: Int -> Int -> [Int]
listFactorsBetween min max = reverse(sort(listFactorsBetween' min min))
    where   listFactorsBetween' :: Int -> Int -> [Int]
            listFactorsBetween' x y
                | x == max && y == max = [x*y]
                | y == max = x*y:listFactorsBetween' (x+1) (x+1)
                | otherwise = x*y:listFactorsBetween' x (y+1) 

-- Return the biggest palindrome got through the products of 2 3-digits numbers
euler4 :: Int
euler4 = euler4' (listFactorsBetween 100 999)
    where 
        euler4' :: [Int] -> Int
        euler4'(x:xs)
            | isPalindrome x = x
            | otherwise = euler4' xs
        isPalindrome :: Int -> Bool
        isPalindrome n = show n == reverse (show n)
