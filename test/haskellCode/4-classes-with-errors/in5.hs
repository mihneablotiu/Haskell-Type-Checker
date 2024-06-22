-- Class instance without usage that has errors
class A a where
    f :: a -> Int

instance A Int where
    f :: Int -> Bool
    f x = x + 1
