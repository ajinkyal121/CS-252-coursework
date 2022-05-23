> import Data.List

Experiment with foldl, foldr, and foldl'

First, implement your own version of the foldl function,
defined as myFoldl

> myFoldl :: (a -> b -> a) -> a -> [b] -> a
> myFoldl f acc [] = acc
> myFoldl f acc (x:xs) = myFoldl f (f acc x) xs 


Next, define a function to reverse a list using foldl.

> myReverse :: [a] -> [a]
> myReverse [] = []
> myReverse (x:xs) = foldl (\acc x-> x : acc) [] xs


Now define your own version of foldr, named myFoldr

> myFoldr :: (a -> b -> b) -> b -> [a] -> b
> myFoldr f acc [] = acc
> myFoldr f acc (x:xs) = f x (myFoldr f acc xs)


Now try using foldl (the library version, not yours) to sum up the numbers of a large list.
Why is it so slow?

foldl creates a reducible expression which gets stored in memory and then gets evaluated.
The reducible expression created here are evaluated lazily.

Instead of foldl, try using foldl'.
Why is it faster?
(Read http://www.haskell.org/haskellwiki/Foldr_Foldl_Foldl%27 for some hints)

foldl' is faster than foldl as foldl' evaluates eagerly instead of lazily as done by foldl.
The reducible expression created here are evaluated one by one which does not store these redex on stack and avoids stack overflow.

For an extra challenge, try to implement foldl in terms of foldr.
See http://www.haskell.org/haskellwiki/Foldl_as_foldr for details.

> foldlUsingfoldr :: (a -> b -> a) -> a -> [b] -> a
> foldlUsingfoldr f a bs =
>  foldr (\b g x -> g (f x b)) id bs a

Next, using the map function, convert every item in a list to its absolute value

> listAbs :: [Integer] -> [Integer]
> listAbs xs = map abs xs

Finally, write a function that takes a list of Integers and returns the sum of
their absolute values.

> sumAbs :: [Integer] -> Integer
> sumAbs xs = foldl' (\acc x -> abs(x) + acc) 0 xs

> main :: IO ()
> main = do
> print(foldl (+) 0 [1..1000000])
> print(foldl' (+) 0 [1..1000000])
> print(foldlUsingfoldr (+) 0 [1..1000000])
> print(listAbs [-1,2,-40,5])
> print(sumAbs [1,3,-5,7])

