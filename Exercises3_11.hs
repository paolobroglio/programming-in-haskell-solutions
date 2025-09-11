-- Exercise 1 what are the types of the following values?
-- ['a', 'b', 'c'] :: [Char]
-- ('a','b','c') :: (Char, Char, Char)
-- [(False, '0'), (True, '1')] :: [(Bool, Char)]
-- [tail, init, reverse] :: [[a] -> [a], [a] -> [a], [a] -> [a]]

-- Exercise 2
-- Write definitions of following types
bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add' :: Int -> Int -> Int -> Int
add' x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f a = f a

-- Exercise 3
-- What are the types of the following functions
second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> b) -> a -> b
twice f x = f (f x)


-- Exercise 5
-- Why is not feasible in general for function types to be instances of Eq class?
-- When it is feasible? Hint: two functions of the same type are equal if they
-- always return equal results for equal arguments
{-
Two functions are equal f == g if and only if f(x) == g(x) for all possible inputs of x.
We can't have a generic definition of this equality since we cannot be sure that for all the inputs x
the functions halts or not, therefore this is an undecidable problem.

We could define functions that are instances of Eq by ourselves but it would work only on that
specific problem.
-}
