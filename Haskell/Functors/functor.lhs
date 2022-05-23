> data Tree v =
>     Empty
>   | Node v (Tree v) (Tree v)
>   deriving (Show)

> findT :: Ord v => v -> Tree v -> Maybe v
> findT _ Empty = Nothing
> findT v (Node val left right) =
>   if val == v then
>     Just val
>   else if v < val then
>     findT v left
>   else
>     findT v right

Your job is to add support for fmap to this tree, so that the following call works:

fmap (+1) (Node 3 (Node 1 Empty Empty) (Node 7 (Node 4 Empty Empty) Empty))

> instance Functor Tree where  
>    fmap f Empty = Empty 
>    fmap f (Node v left right) = Node (f v) (fmap f left) (fmap f right)