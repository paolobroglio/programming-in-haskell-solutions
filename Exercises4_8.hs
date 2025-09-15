-- Exercise 1
-- using library functions define a function halve :: [a] -> ([a], [a]) that splits
-- an even-lengthed list into two halves
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
          where 
            half = (length xs) `div` 2

-- Exercise 2
-- Define a function third :: [a] -> a that returns the third element in a list
-- that contains at least this many elements using:
-- a. head and tail
-- b. list indexing !!
-- c. pattern matching
thirdA :: [a] -> a
thirdA xs = head (tail (tail xs))

thirdB :: [a] -> a
thirdB xs = xs !! 2

thirdC :: [a] -> a
thirdC (_ : (_ : (a : xs))) = a

-- Exercise 3
-- Given safetail :: [a] -> [a] that behaves in the same way as tail except that it maps
-- the empty list to itself rather than producing an error. Using tail and the function
-- null :: [a] -> Bool that decides if a list is empty or not, define safetail using:
-- a. a conditional expression
-- b. guarded equations
-- c. pattern matching
safetailA :: [a] -> [a]
safetailA xs = if (null xs) then [] else tail xs

safetailB :: [a] -> [a]
safetailB xs | null xs = []
             | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC xs = tail xs

-- Exercise 4 
-- Show how the disjunction operator || can be defined in four different ways using
-- pattern matching
(||) :: Bool -> Bool -> Bool
True || _ = True
False || b = b

-- Exercise 5-6
-- Formalise && using conditional expressions
and' :: Bool -> Bool -> Bool
and' a b = if a == False then a else 
          if b == False then b else a

and2 :: Bool -> Bool -> Bool
and2 a b = if a == True then b else False

-- Exercise 7
-- Formalise the following curried function in terms of lambda expressions
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

multL :: Int -> Int -> Int -> Int
multL = \x -> (\y -> (\z -> x * y * z))

-- Exercise 8
-- Define a function luhnDouble :: Int -> Int that double a digit and subtracts 9 if the result is greater than 9
-- Define luhn :: Int -> Int -> Int -> Int that decides if a bank card number is valid
luhnDouble :: Int -> Int
luhnDouble n = if res > 9 then res - 9 else res
            where 
              res = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = divisibleByTen total
              where
                total = (luhnDouble a) + b + (luhnDouble c) + d
                divisibleByTen = \x -> mod x 10 == 0
