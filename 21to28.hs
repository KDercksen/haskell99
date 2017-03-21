import Control.Applicative
import Control.Monad (replicateM)
import Data.List
import Data.Ord
import System.Random

-- 21. Insert an element at a given position in a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (a, b) = splitAt (n - 1) xs in a ++ x:b

-- 22. Create a list containing all integers in a given range.
range a b = [a..b]

-- 23. Extract a given number of randomly selected elements from a list.
rndSelect :: [a] -> Int -> IO [a]
rndSelect [] _ = return []
rndSelect xs n
    | n < 0 = error "N must be greater than zero."
    | otherwise = do
        pos <- replicateM n $
            getStdRandom $ randomR (0, (length xs) - 1)
        return [xs !! p | p <- pos]

-- 24. Draw N different random numbers from the set 1..M.
diffSelect :: Int -> Int -> IO [Int]
diffSelect n m = take n . nub . randomRs (1, m) <$> getStdGen

-- 25. Generate a random permutation of the elements of a list.
rndElem :: [a] -> IO a
rndElem xs = do
    index <- randomRIO (0, length xs - 1)
    return $ xs !! index

rndPermu :: [a] -> IO [a]
rndPermu xs = rndElem . permutations $ xs

-- 26. Generate the combinations of K distinct objects chosen from the N
-- elements of a list.
combinations k xs = filter ((k==) . length) (subsequences xs)

-- 27. Group the elements of a set into disjoint subsets.
--
-- a. In how many ways can a group of 9 people work in 3 disjoint subgroups of
-- 2, 3 and 4 persons? Write a function that generates all the possibilities
-- and returns them in a list.
--
-- b. Generalize the above predicate in a way that we can specify a list of
-- group sizes and the predicate will return a list of groups.
combination :: Int -> [a] -> [([a], [a])]
combination 0 xs = [([], xs)]
combination n [] = []
combination n (x:xs) = ts ++ ds
    where
        ts = [(x:ys, zs) | (ys, zs) <- combination (n - 1) xs]
        ds = [(ys, x:zs) | (ys, zs) <- combination n xs]

mkGroup :: [Int] -> [a] -> [[[a]]]
mkGroup [] xs = [[]]
mkGroup (g:gs) xs = concatMap helper $ combination g xs
    where helper (as, bs) = map (as:) (mkGroup gs bs)

-- 28. Sorting a list of lists to length of sublists.
lsort = sortBy $ comparing length
