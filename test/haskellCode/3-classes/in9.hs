class A a where
    f1 :: a -> Bool

instance A Int where
    f1 :: Int -> Bool
    f1 x = True

instance A Bool where
    f1 :: Bool -> Bool
    f1 x = False

testTwoFuncs :: Int -> Bool
testTwoFuncs x = f1 (f1 x)
