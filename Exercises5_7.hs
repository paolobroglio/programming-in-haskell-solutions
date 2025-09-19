-- Exercise 1
-- using a list comprehension give an expression that calculates
-- the sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares
oneHundredSquaresSum :: Int
oneHundredSquaresSum = sum [x^2 | x <- [1..100]]

-- Exercise 2
-- Suppose a coordinate grid of size m x n is given by the list of all pairs (x,y)
-- of integers such that 0 <= x <= m and 0 <= y <= n. Using a list comprehension
-- define a function grid :: Int -> Int -> [(Int, Int)] that returns a coordinate grid
-- of a given size.
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- Exercise 3 
-- using the grid function define a function square :: Int -> [(Int, Int)] that returns a coordinate
-- square of size n, excluding the diagonal from (0,0) to (n,n)
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- Exercise 4
-- In a similar way to length function show how the function replicate :: Int -> a -> [a] can be 
-- defined using a list comprehension
replicate :: Int -> a -> [a]
replicate n e = [e | _ <- [1..n]]

-- Exercise 5
-- A triple (x, y, z) is Pythagorean if it satisfies the equation x^2 + y^2 = z^2. Using a list 
-- comprehension with three generators define pyths :: Int -> [(Int, Int, Int)] that returns the list
-- of all such triples whose components are at most a given limit.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- Exercise 6
-- A positive integer is perfect if it equals the sum of all of its factors excluding the number itself.
-- use a list comprehension and the function factors to define a function perfects :: Int -> [Int] that
-- returns the list of all perfect numbers up to a given limit
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) - x == x]

-- Excercise 7 
-- Show [(x,y) | x <- [1,2], y <- [3,4]] can be expressed in two list comprehensions with single
-- generators
-- [x | x <- concat [[(1,y) | y <- [3,4]],[(2,y) | y <- [3,4]]]]
--

-- Exercise 8 
-- Redefine function positions using the function find
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- positions :: Eq a => a -> [(a,b)] -> [Int]
-- positions x xs = [position | position <- find x [fst t | t <- xs]]
