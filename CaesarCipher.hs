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
