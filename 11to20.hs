import Data.List

-- 11. Modified run-length encoding. Modify the result of problem 10 in such a
-- way that if an element has no duplicates it is simply copied into the result
-- list. Only elements with duplicates are transferred as (N E) lists.
data Number a = Multiple Int a | Single a deriving (Show)

encode xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

-- 12. Decode a run-length encoded list.
decode :: [Number a] -> [a]
decode = concatMap decodeHelper
    where
        decodeHelper (Single x) = [x]
        decodeHelper (Multiple n x) = replicate n x

-- 13. Run-length encoding of a list (direct solution).
encode' :: Eq a => [a] -> [(Int, a)]
encode' = foldr helper []
    where
        helper x [] = [(1, x)]
        helper x (y@(a, b):ys)
            | x == b = (1 + a, x):ys
            | otherwise = (1, x):y:ys

encodeDirect :: Eq a => [a] -> [Number a]
encodeDirect = map encodeHelper . encode'
    where
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x

-- 14. Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

-- 15. Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

-- 16. Drop every N'th element from a list.
dropEvery xs n = [i | (i, c) <- (zip xs [1, 2..]), (mod c n) /= 0]

-- 17. Split a list in two parts; the length of the first part is given.
split = flip $ splitAt

-- 18. Extract a slice from a list.
slice xs a b | a > 0 = take (b - a + 1) $ drop (a - 1) xs

-- 19. Rotate a list N places to the left.
rotate xs n = take (length xs) $ drop (length xs + n) $ cycle xs

-- 20. Remove the K'th element from a list.
removeAt n xs = (xs !! (n - 1), take (n - 1) xs ++ drop n xs)
