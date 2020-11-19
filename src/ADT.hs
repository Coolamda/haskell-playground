module ADT where

data Either a b = Left a 
                | Right b 
                deriving (Read, Eq, Show, Ord)

data Tree a = EmptyTree 
            | Node a (Tree a) (Tree a) 
            deriving (Show, Read)

sampleTree :: Tree Int
sampleTree = foldr treeInsert EmptyTree [1..10]

singleton :: a -> Tree a
singleton n = Node n EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a =  Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x >  a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red    = True
    Yellow == Yellow = True
    Green  == Green  = True
    _      == _      = False

instance Show TrafficLight where
    show Red    = "Red light"
    show Yellow = "Yellow light"
    show Green  = "Green light"

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = False

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance Functor Tree where
    fmap _ EmptyTree           = EmptyTree
    fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
