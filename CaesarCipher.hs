import Data.Char

-- let2int converts a lower-case letter between 'a' and 'z' into the corresponding integer between 0 and 25
let2int :: Char -> Int
let2int c = ord c - ord 'a'
-- int2let converts an integer between 0 and 25 to a lower-case letter between 'a' and 'z'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

-- shift applies a shift factor to a lower-case letter by converting the letter to the corresponding integer,
-- adding the shift factor and taking the remainder when divided by 26, and converting the result back into a char
shift :: Int -> Char -> Char
shift factor letter | isLower letter = int2let ((let2int letter + factor) `mod` 26)
                    | otherwise = letter

-- encode applies shift over a list of chars or a string
encode :: Int -> String -> String
encode factor xs = [shift factor (toLower x) | x <- xs]

-- decode just applies the encode function with negative factor to shift back the letters
decode :: Int -> String -> String
decode factor xs = encode (- factor) xs

-- CRACKING THE CAESAR CIPHER
-- Frequency analysis is the easiest way to break this cipher. An english language character frequency table can be used to have a best guess on what is hiding behind a ciphered text
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2,
         0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0,
         2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

count :: Char -> String -> Int
count c xs = length [x | x <- xs, x == c]


percentage :: Int -> Int -> Float
percentage n m = (fromIntegral n / fromIntegral m) * 100

frequencies :: String -> [Float]
frequencies xs = [percentage (count x xs) n | x <- ['a'..'z']]
                 where n = length xs

-- Use Chi Square functions to compare a list of observed frequencies with a list of expected frequencies
-- A smaller value means a better match
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]


positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = encode (-factor) xs
  where 
	factor = head (positions (minimum chitab) chitab)
	chitab = [chisqr (rotate n table') table | n <- [0..25]]
	table' = frequencies xs
