module Main where 

import Data.Set (set, Set, (<$!>), (<!>), (>>=!), (|>), (<|))

assert :: String -> Bool -> IO()
assert _ True = putStr ""
assert a False = putStrLn $ "Test Failed: " ++ a

main :: IO()
main = do
    assert "Two set constructions should equal each other" $ (set [1, 2, 3]) == (set [1, 2 ,3])
    assert "A set constructed with duplicates should remove them after" $ (set [1, 1, 2, 2, 3, 3]) == (set [1, 2, 3])
    assert "A set should be fmappable" $ (set [2, 3, 4]) == ((\ a -> a + 1) <$!> (set [1, 2, 3]))
    assert "A set should be associatable" $ (set [1, 2, 3]) == ((set [1, 2, 3]) <!> (set [1, 2, 3]))
    assert "A set should be work pretty much like a Monad" $ (set [2, 3, 4]) == ((set [1, 2, 3]) >>=! (\ a -> set [(a + 1)]))
    assert "A set can be added to and removed one at a time" $ (==) (set [1, 2, 4]) $ (set [] :: Set Int) |> 4 |> 1 |> 2 |> 2 |> 3 <| 3 
