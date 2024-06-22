class A a where
    f :: a -> Int
    g :: Int -> a
    h :: a -> a

instance A Int where
    f :: Int -> Int
    f x = x + 1

    g :: Int -> Int
    g x = x + 2

    h :: Int -> Int
    h x = x + 3

testFunction :: Int -> Int
testFunction x = (h (g (f x)))
