import Data.Set
import Control.Monad
 
powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList
 
listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

reflex :: Set String -> Set (Set String, Set String)
reflex set | set == empty = empty
           | otherwise = let part = Prelude.map concat $ Prelude.map (toList) (toList  (delete empty $ powerset set))
                             set' = concat $ toList set
                         in fromList $ Prelude.map (\x -> (set',x)) part 
                         
-- regla de reflexividad
closeRef :: Set String -> Set (String,String) -> Set (String,String)
closeRef r f = let pr = powerset r
                   aux = Data.Set.map reflex pr
               in concatSet aux
            

concatSet :: Set (Set (String,String)) -> Set (String,String)
concatSet set | set == empty = empty
              | otherwise = let a = toList set
                            in f a
               where f []     = empty
                     f (x:xs) = union x (f xs)

-- regla de aumento{-
{-
close :: Set String -> Set (String,String) -> Set (String,String)
close r f = let aux
-}