{- |
Module                  : Lecture1
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 1 of the Haskell Beginners course.

To complete the exercises, you need to complete the implementation of all functions
and add the missing top-level type signatures. You are free to implement additional
helper functions. But you can't change the names of the given functions.

Comments before each function contain explanations and example of
arguments and expected returned values.

It's absolutely okay (and perhaps to be expected) if you feel that your solutions are not
perfect. You can return to these exercises at a later stage and
improve your solutions if you see any room for improvement.
-}

module Lecture1
    ( makeSnippet
    , sumOfSquares
    , lastDigit
    , minmax
    , subString
    , strSum
    , lowerAndGreater
    ) where

{- | Specify the type signature of the following function. Think about
its behaviour, possible types for the function arguments and write the
type signature explicitly.
-}
makeSnippet :: Int -> String -> String
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

{- | Implement a function that takes two numbers and finds sum of
their squares. Also, make sure to add a full type signature for the
function.

>>> sumOfSquares 3 4
25

>>> sumOfSquares (-2) 7
53

Explanation: @sumOfSquares 3 4@ should be equal to @9 + 16@ and this
is 25.
-}
sumOfSquares :: Num a => a -> a -> a
sumOfSquares x y = x^2 + y^2

{- | Implement a function that returns the last digit of a given number. Again, also add the
type signature for the function.

>>> lastDigit 42
2
>>> lastDigit (-17)
7

🕯 HINT: use the @mod@ function

-}
lastDigit :: Integral a => a -> a
lastDigit n = abs n `mod` 10

{- | Write a function that takes three numbers and returns the
difference between the biggest number and the smallest one.

>>> minmax 7 1 4
6

Explanation: @minmax 7 1 4@ returns 6 because 7 is the biggest number
and 1 is the smallest, and 7 - 1 = 6.

Try to use local variables (either let-in or where) to implement this
function.
-}

minmax :: (Num a, Ord a) => a -> a -> a -> a
minmax x y z = let mi = min x $ min y z
                   ma = max x $ max y z
                in ma - mi

{- | Implement a function that takes a string, a start and an end position
and returns a substring of a given string from the start position to
the end (including).

>>> subString 3 7 "Hello, world!"
"lo, w"

>>> subString 10 5 "Some very long String"
""

This function can accept a negative start and end position. A negative
start position should be considered as zero (e.g. substring from the
first character) and a negative end position should result in an empty
string.
-}
subString :: Int -> Int -> [a] -> [a]
subString start end str = let end' = max 0 end
                              start' = max 0 start
                              len = if end < 0 then 0 else end' - start' + 1
                           in take len $ drop start' str

{- | Write a function that takes a String — space separated numbers,
and finds a sum of the numbers inside this string.

>>> strSum "100    -42  15"
73

The string contains only spaces and/or numbers.
-}
strSum :: String -> Int
strSum str = sum $ map (read) $ words str

{- | Write a function that takes a number and a list of numbers and
returns a string, saying how many elements of the list are strictly
greated than the given number and strictly lower.

>>> lowerAndGreater 3 [1 .. 9]
"3 is greater than 2 elements and lower than 6 elements"

Explanation: the list [1 .. 9] contains 9 elements: [1, 2, 3, 4, 5, 6, 7, 8, 9]
The given number 3 is greater than 2 elements (1 and 2)
and lower than 6 elements (4, 5, 6, 7, 8 and 9).

🕯 HINT: Use recursion to implement this function.
-}
lowerAndGreater :: (Num a, Ord a, Show a) => a -> [a] -> [Char]
lowerAndGreater n list = let (smallerCount, biggerCount) = partitionCount n list
    in
        show n ++ " is greater than " ++ show smallerCount ++ " elements and lower than " ++ show biggerCount ++ " elements"


partitionCount :: (Num a, Ord a, Num b) => a -> [a] -> (b, b)
partitionCount _ [] = (0,0)
partitionCount n (h:r) = let (s, b) = partitionCount n r
                            in if n > h then (s + 1, b)
                               else if n < h then (s, b + 1)
                               else (s, b)
