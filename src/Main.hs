import Data.List (sort)

newtype Set a = Set (Maybe [a]) deriving (Show, Eq, Ord)

instance Functor Set where
    f `fmap` (Set Nothing) = Set Nothing
    f `fmap` (Set (Just a)) = Set $ Just $ (\ b -> f b) <$> a

instance Applicative Set where
    pure a = Set (Just [a])
    (Set Nothing) <*> _ = Set Nothing
    _ <*> (Set Nothing) = Set Nothing
    (Set (Just f)) <*> (Set (Just g)) = Set $ Just $ f <*> g

foldr_ :: (Set a) -> (Set a) -> (Set a)
foldr_ (Set Nothing) (Set (Just a)) = Set $ Just a
foldr_ (Set (Just a)) (Set Nothing) = Set $ Just a
foldr_ (Set Nothing) (Set Nothing) = Set Nothing
foldr_ (Set (Just a)) (Set (Just b)) = Set $ Just $ a <> b

instance Monad Set where
    return = pure
    (Set Nothing) >>= _ = Set Nothing
    (Set (Just a)) >>= f = foldr foldr_ (Set (Just [])) $ (\ d -> f d) <$> a

dedup :: (Ord a, Eq a) => [a] -> [a]
dedup a = dedup_ (sort a) []
dedup_ :: (Ord a, Eq a) => [a] -> [a] -> [a]
dedup_ [] a = a
dedup_ a [] = dedup_ (tail a) [head a]
dedup_ a b = dedup_ (tail a) $ dedup__ b c $ c == (head (reverse b))
    where c = head a
dedup__ :: (Ord a, Eq a) => [a] -> a -> Bool -> [a]
dedup__ a _ True = a
dedup__ a b False = a <> [b]

instance (Ord a, Eq a) => Semigroup (Set a) where
    (Set (Just a)) <> (Set Nothing) = Set $ Just a
    (Set Nothing) <> (Set (Just a)) = Set $ Just a
    (Set (Just a)) <> (Set (Just b)) = Set $ Just $ dedup $ a <> b

set :: (Ord a, Eq a, Show a) => a -> Set a
set = return 

main :: IO()
main = do
    putStrLn $ show $ Set $ Just [1, 2, 3] 
    putStrLn $ show $ (\ a -> a + 1) <$> (pure 1 :: Set Int)
    putStrLn $ show $ pure 2 <> (pure 1 :: Set Int)
    putStrLn $ show $ set 2 <> set 1
    putStrLn $ show $ dedup [2, 1, 2, 3, 4]
    putStrLn $ show $ set 2 <> set 1 <> set 2
