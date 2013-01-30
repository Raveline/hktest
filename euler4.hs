listPalindromes :: [Int]
listPalindromes = euler4' 999 999
    where
        euler4' :: Int -> Int -> [Int]
        euler4' x y 
            | isPalindrome (x*y) = (x*y):euler4' (x-1) y
            | y == 500 = []
            | x > 99 = euler4' (x-1) y
            | otherwise = euler4' 999 (y-1)
        
        isPalindrome :: Int -> Bool
        isPalindrome n = show n == reverse (show n)

euler4 :: Int
euler4 = maximum(listPalindromes)
