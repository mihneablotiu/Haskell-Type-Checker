class B b where
    g :: b -> b

instance B Int where
    g :: Int -> Int
    g x = x + 1

instance B a where
    g :: a -> a
    g x = x

testFunc :: Int -> Int
testFunc x = g x
