class Ord a where
    compare :: a -> a -> Bool

instance Ord Int where
    compare :: Int -> Int -> Bool
    compare x y = True

ordFunc :: Int -> Int -> Bool
ordFunc x y = compare x y
