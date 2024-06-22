-- Usage of (==) without Bool instance
class A a where
    (==) :: a -> a -> Bool

instance A Int where
    (==) :: Int -> Int -> Int
    (==) x y = 0

testFunc :: Bool -> Bool -> Bool
testFunc x y = (==) x y
