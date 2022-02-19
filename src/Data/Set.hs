module Data.Set (set, Set, (<#>), (<$!>), fmapSet, (<!>), associative, (>>=!), sequence', (|>), add, (<|), remove) where

import Data.List (sort)

newtype Set a = Set [a] deriving (Show, Eq, Ord)

type Constraint a = (Eq a, Ord a)

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

set :: Constraint a => [a] -> Set a
set a = Set $ dedup a

infixl 4 <#>
(<#>) :: (Functor f) => f a -> (a -> b) -> f b
(<#>) = fmap'
fmap' :: (Functor f) => f a -> (a -> b) -> f b
a `fmap'` f = f <$> a

infixl 4 <$!>
(<$!>) :: (Constraint a, Constraint b) => (a -> b) -> Set a -> Set b
(<$!>) = fmapSet
fmapSet :: (Constraint a, Constraint b) => (a -> b) -> Set a -> Set b
f `fmapSet` (Set a) = set $ a <#> \ b -> f b

infixr 6 <!>
(<!>) :: (Constraint a) => Set a -> Set a -> Set a
(<!>) = associative
associative :: (Constraint a) => Set a -> Set a -> Set a
associative (Set a) (Set b) = set $ a <> b

infixl 1 >>=!
(>>=!) :: (Constraint a, Constraint b) => Set a -> (a -> Set b) -> Set b
(>>=!) = sequence'
sequence' :: (Constraint a, Constraint b) => Set a -> (a -> Set b) -> Set b
(Set a) `sequence'` f = foldr (\ a b -> a <!> b) (set []) $ (\ b -> f b) <$> a

infixl 2 |>
(|>) :: Constraint a => Set a -> a -> Set a
(|>) = add
add :: Constraint a => Set a -> a -> Set a
a `add` b = a <!> set [b]

infixl 2 <|
(<|) :: Constraint a => Set a -> a -> Set a
(<|) = remove
remove :: Constraint a => Set a -> a -> Set a
(Set a) `remove` b = set $ (\ c -> b /= c) `filter` a
