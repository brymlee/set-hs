module Data.Set (Set, set, (<|), (|>)) where

import Data.List (sort)

newtype Set a = Set [a] deriving (Show, Eq, Ord)

instance Functor Set where
    f `fmap` (Set a) = Set $ f <$> a

instance Applicative Set where
    pure a = Set [a]
    (Set f) <*> (Set a) = Set $ f <*> a

instance Monad Set where
    return = pure
    (Set a) >>= f = foldr (\ (Set a) (Set b) -> Set (a <> b)) (Set []) $ f <$> a

instance Constraint a => Semigroup (Set a) where
    (Set a) <> (Set b) = Set $ dedup $ a <> b

type Constraint a = (Show a, Eq a, Ord a)

dedup :: Constraint a => [a] -> [a]
dedup a = dedup_ (sort a) []
dedup_ :: Constraint a => [a] -> [a] -> [a]
dedup_ [] a = a
dedup_ a [] = dedup_ (tail a) [head a]
dedup_ a b = dedup_ (tail a) $ dedup__ b c $ c == (head (reverse b))
    where c = head a
dedup__ :: Constraint a => [a] -> a -> Bool -> [a]
dedup__ a _ True = a
dedup__ a b False = a <> [b]

infixl 2 |>
(|>) :: Constraint a => Set a -> a -> Set a
(|>) = add
add :: Constraint a => Set a -> a -> Set a
a `add` b = a <> set [b]

infixl 2 <|
(<|) :: Constraint a => Set a -> a -> Set a 
(<|) = remove
remove :: Constraint a => Set a -> a -> Set a
(Set a) `remove` b = set $ (\ c -> b /= c) `filter` a

set :: Constraint a => [a] -> Set a
set a = Set $ dedup a
