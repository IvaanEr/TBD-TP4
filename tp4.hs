module TP4 where 

import Data.Set
import Control.Monad
 
powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList
 
listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

reflex :: Set String -> Set (Set String, Set String)
reflex set | set == empty = empty
           | otherwise = let part = delete empty $ powerset set 
                         in Data.Set.map (\x -> (set,x)) part 

-- regla de reflexividad
closeRef :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeRef r f = let pr = powerset r
                   aux = unions $ toList $ Data.Set.map reflex pr
               in union f aux
            

concatSet :: Set (Set (String,String)) -> Set (String,String)
concatSet set | set == empty = empty
              | otherwise = let a = toList set
                            in f a
               where f []     = empty
                     f (x:xs) = union x (f xs)

-- regla de aumento	

closeAum :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeAum r f = let pr = delete empty $ powerset r
               in  Data.Set.map (\rr -> Data.Set.map (\(x,y)-> (union x rr, union y rr)) f ) pr