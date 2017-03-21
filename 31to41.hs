import Data.List

-- 31. Determine whether a given integer number is prime.
isPrime n = not $ any divisible $ takeWhile notTooBig [2..]
    where
        divisible y = mod n y == 0
        notTooBig y = y*y <= n

-- 32. Determine the greatest common divisor of two integer numbers. Use
-- Euclid's algorithm.
myGCD x y
    | y == 0 = abs x
    | otherwise = myGCD y (mod x y)

-- 33. Determine whether two positive integer numbers are coprime. Two numbers
-- are coprime if their greatest common divisor equals 1.
coprime x y = gcd x y == 1

-- 34. Calculate Euler's totient function phi(m).
--
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integer r (1 <= r < m) that are coprime to m.
--
-- Example: m = 10, r = 1, 3, 7, 9; thus phi(m) = 4. Note the special case
-- phi(1) = 1.
totient m = length [x | x <- [1..m], coprime x m]

-- 35. Determine the prime factors of a given positive integer. Construct a
-- flat list containing the prime factors in ascending order.
factor :: Int -> [Int]
factor 1 = []
factor n = let divs = dropWhile ((/= 0) . mod n) [2..ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divs then n else head divs
           in (prime:) $ factor $ div n prime

-- 36. Determine the prime factors of a given positive integer. Construct a
-- list containing the prime factors and their multiplicity.
mulFactor = map encode . group . factor
    where
        encode xs = (head xs, length xs)

-- 37. Calculate Euler's totient function phi(m) (improved).
totient2 m = product [(p - 1) * p ^ (c - 1) | (p, c) <- mulFactor m]

-- 38. Compare the two methods of calculating Euler's totient function.
--
-- totient2 is EXTREMELY fast compared to the naive implementation.

-- 39. A list of prime numbers.
primesR a b = [x | x <- [a..b], isPrime x]

-- 40. Goldbach's conjecture.
--
-- Goldbach's conjecture says that every positive even number greater than 2 is
-- the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
-- famous facts in number theory that has not been proved to be correct in the
-- general case. It has been numerically confirmed up to very large numbers
-- (much larger than we can go with our system). Write a predicate to find the
-- two prime numbers that sum up to a given even integer.
goldbach n = head [(x, y) | x <- primesR 2 (n - 2), let y = n - x, isPrime y]

-- 41. Given a range of integers by its lower and upper limit, print a list of
-- all even numbers and their Goldbach composition.
goldbachList n m = map goldbach $ dropWhile (<4) $ filter even [n..m]
goldbachList' n m i = filter (\(x, y) -> x > i && y > i) $ goldbachList n m
