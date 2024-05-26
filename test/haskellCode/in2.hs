class A a where
    f :: a -> Bool

instance A Int where
    f :: Int -> Bool
    f x = True

foo :: Int -> Bool
    foo x = f x