class B b where
    g :: b -> b

instance B Int where
    g :: Int -> Int
    g x = x + 1
