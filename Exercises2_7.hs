-- Exercise 3
-- The script below contains 3 syntactic errors. Correct these errors and then check that
-- your script works properly using GHCi.
{-

N = a `div` length xs
    where
        a = 10
       xs = [1,2,3,4,5]
-}
n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

-- Exercise 4
-- Show how the function last could be defined in terms of the other functions
-- introduced in this chapter.
myLast xs = head (reverse xs)

-- Exercise 5
-- init removes the last element of a non-empty list. How can it be defined in two different
-- ways?
myInitOne xs = take (length xs - 1) xs
myInitTwo xs = reverse (drop 1 (reverse xs))
myInitThree xs = reverse (tail (reverse xs))
