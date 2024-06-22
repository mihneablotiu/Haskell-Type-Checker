class A a where
    f :: a -> Int

instance A Int where
    f :: Int -> Int
    f x = x + 2

foo :: Int -> Int
foo y = f y
