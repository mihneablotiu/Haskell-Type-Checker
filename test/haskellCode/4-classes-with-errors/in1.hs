-- Wrong type of sum in instance declaration
class B b where
    g :: b -> b

instance B Int where
    g :: Int -> Bool
    g x = x + 1