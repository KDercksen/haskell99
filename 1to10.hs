import Data.List

-- 1. Find the last element of a list.
myLast x = last x

-- 2. Find the last but one element of a list.
lastButOne = last . init

-- 3. Find the K'th element of a list. The first element is number 1.
elementAt :: [a] -> Int -> a
elementAt list i = list !! (i - 1)

-- 4. Find the number of elements in a list.
myLength x = length x

-- 5. Reverse a list.
myReverse x = reverse x

-- 6. Find out whether a list is a palindrome.
isPalindrome x = and $ zipWith (==) x (reverse x)

-- 7. Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

-- 8. Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress = map head . group

-- 9. Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [[a]]
pack = group

-- 10. Run-length encoding of a list. Use the result of problem 9 to implement
-- the so-called run-length encoding data compression method. Consecutive
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group
