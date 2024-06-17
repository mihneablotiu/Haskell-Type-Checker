applyTwice :: (Int -> Int) -> Int -> Int
applyTwice f x = f (f x)

result :: Int
result = applyTwice 5 5
