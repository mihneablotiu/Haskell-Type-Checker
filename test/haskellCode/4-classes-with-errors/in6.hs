-- Class instance function correctly defined but with wrong type signature matching the class function
class A a where
    f :: a -> Int

instance A Int where
    f :: Int -> Bool
    f x = True
