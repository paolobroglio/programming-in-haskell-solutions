-- 1 Give another possible calculation of the result of double (double 2)
-- double (double 2)
-- double (2 + 2)
-- (2 + 2) + (2 + 2)
-- 4 + (2 + 2)
-- 4 + 4
-- 8

-- 2 Show that sum [x] = x for any number x
mysum [] = 0
mysum (n:ns) = n + mysum ns

-- sum [x]
-- x + sum []
-- x + 0
-- x

-- 3 Define a function product that produces the product of a list of numbers,
-- and show using your definition that product [2,3,4] == 24
myproduct [] = 1
myproduct [n:ns] = n * myproduct [ns]

-- product [2,3,4]
-- 2 * product[3,4]
-- 2 * 3 * product [4]
-- 2 * 3 * 4 * product []
-- 2 * 3 * 4 * 1
-- 24

-- 4 How should the definition of the function qsort be modified so that it produces a reverse sorted
-- version of a list?
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                 smaller = [a | a <- xs, a <= x]
                 larger = [b | b <- xs, b > x]

qsortreverse [] = []
qsortreverse (x:xs) = qsortreverse larger ++ [x] ++ qsortreverse smaller
                      where
                        smaller = [a | a <- xs, a <= x]
                        larger = [b | b <- xs, b > x]

-- 5 What would be the effect of replacing <= by < in the original definition of qsort?
-- Hint: consider the example qsort [2,2,3,1,1]
-- qsort [2,2,3,1,1]
-- qsort [1, 1] ++ [2] ++ qsort [3]
-- qsort [] ++ [1] ++ qsort [] ++ [2] ++ qsort [3]
-- [] ++ [1] ++ [] ++ [2] ++ qsort [] ++ [3] ++ qsort []
-- [] ++ [1] ++ [] ++ [2] ++ [] ++ [3] ++ []
-- [1, 2, 3]

-- If there are equal elements in the remaining elements xs they will not be included
-- neither in the smaller nor in the larger portions therefore there will be not duplicated elements
-- in the resulting list
