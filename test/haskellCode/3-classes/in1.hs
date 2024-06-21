class A a where
    (==) :: a -> a -> Bool

instance A Bool where
    (==) :: Bool -> Bool -> Bool
    (==) x y = True

testFunc :: Bool -> Bool -> Bool
testFunc x y = (==) x y
