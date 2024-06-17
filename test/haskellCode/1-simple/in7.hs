applyTwice :: (Int -> Int) -> Int -> Int
applyTwice f x = f (f x)

increment :: Int -> Int
increment x = x + 1

result :: Int
result = applyTwice increment 5
