add :: Int -> Int -> Int
add x y = x + y

partialApply :: (Int -> Int -> Int) -> Int -> Int -> Int
partialApply f x = f 7
