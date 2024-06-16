double :: Int -> Int
double x = x + x

increment :: Int -> Int
increment y = y + 1

result :: Int
result = double (increment True)
