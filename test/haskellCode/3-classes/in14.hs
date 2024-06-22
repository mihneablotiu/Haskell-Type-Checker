class A a where
    f :: a
    g :: a

instance A Int where
    f :: Int
    f = 1

    g :: Int
    g = 2

h :: Int
h = f + g
