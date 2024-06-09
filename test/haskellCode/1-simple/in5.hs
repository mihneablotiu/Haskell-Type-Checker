-- Multiple function applications
add :: Int -> Int -> Int
add x y = x + y

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = add x (add y z)
