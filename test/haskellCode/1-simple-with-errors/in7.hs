add :: Bool -> Bool -> Int
add x y = x + y

partialApply :: (Int -> Bool -> Int) -> Bool -> Int -> Int
partialApply f x = f 7
