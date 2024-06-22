-- Wrong actual parameter for sum in double
double :: Bool -> Int
double x = x + x

increment :: Int -> Int
increment y = y + 1

result :: Int
result = incremet (double True)
