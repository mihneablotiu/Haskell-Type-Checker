class A a where
    f1 :: a -> Bool

class B b where
    f2 :: b -> Int

instance A Int where
    f1 :: Int -> Bool
    f1 x = True

instance B Bool where
    f2 :: Bool -> Int
    f2 x = 1

testTwoFuncs :: Int -> Int
testTwoFuncs x = f2 (f1 x)
