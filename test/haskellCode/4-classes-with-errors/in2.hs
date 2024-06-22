-- Usage of (==) without any instance
class A a where
    (==) :: a -> a -> Bool

testFunc :: Bool -> Bool -> Bool
testFunc x y = (==) x y