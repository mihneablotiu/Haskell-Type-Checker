-- Multiple definitions of add
add :: Int -> Int -> Int
add x y = x + y

add :: Bool -> Bool -> Int
add x y = x + y

partialApply :: (Int -> Int -> Int) -> Int -> Int -> Int
partialApply f x = add 7
