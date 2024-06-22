-- Defining a class with a function that has the same name as a function in the global scope (should throw an error)
f :: Int -> Int
f x = x

class A a where
    f :: a -> Int

instance A Int where
    f :: Int -> Int
    f x = x + 1

testFunc :: Int -> Int
testFunc x = f x
