import Data.Set
import Control.Monad
 
powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList
 
listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])


powerset [] = [[]]
powerset (head:tail) = acc ++ map (head:) acc where acc = powerset tail

powerset = foldr (\x acc -> acc ++ map (x:) acc) [[]]