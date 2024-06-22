class A a where
    f :: a -> Int

instance A Int where
    f :: Int -> Int
    f x = x + 1

testFunc :: Int -> Int
testFunc x = f (f (f x))
