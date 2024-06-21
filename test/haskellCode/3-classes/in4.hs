class A a where
    f :: a -> a -> Bool

instance A Bool where
    f :: Bool -> Bool -> Bool
    f x y = True

class B b where
    g :: b -> b -> Bool

instance B Bool where
    g :: Bool -> Bool -> Bool
    g = f
