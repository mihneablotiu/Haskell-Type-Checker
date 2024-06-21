double :: Int -> Int
double x = x + x

class Constants a where
    twoSquared :: a

instance Constants Int where
    twoSquared :: Int
    twoSquared = double 2
