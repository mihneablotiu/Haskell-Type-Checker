class A a where
    (==) :: a -> a -> Bool

instance A Bool where
    (==) :: Bool -> Bool -> Bool
    (==) x y = True

testFunc :: Int -> Int -> Bool -> Bool
testFunc x y = (==) True
