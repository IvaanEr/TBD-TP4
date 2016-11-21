module TP where 

import Data.Set
import Control.Monad

niceShow f = toList  $ Data.Set.map (\(x,y) -> (concat $ toList  x,concat $ toList  y)) f


entryR :: String -> Set String
entryR xs = Prelude.foldr (\x -> union $ fromList [[x]]) empty xs

entryF :: [(String,String)] -> Set (Set String,Set String)
entryF xs = fromList (Prelude.map (\(x,y) -> (entryR x, entryR y)) xs )

powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList
 
listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

reflex :: Set String -> Set (Set String, Set String)
reflex set | set == empty = empty
           | otherwise = let part = delete empty $ powerset set 
                         in Data.Set.map (\x -> (set,x)) part 

-- regla de reflexividad
closeRef :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeRef pr f = let aux = unions $ toList $ Data.Set.map reflex pr
                in union f aux

-- regla de aumento 
closeAum :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeAum pr f = unions $ toList $ Data.Set.map (\rr -> Data.Set.map (\(x,y)-> (union x rr, union y rr)) f ) pr

-- regla de transitividad
closeTrans :: Set (Set String,Set String) -> Set (Set String,Set String)
closeTrans f = unions $ toList $ Data.Set.map (\(a1,b1)-> Data.Set.map (\(a2,b2)-> if b1 == a2 then (a1,b2) else (a2,b2)) f) f


closeSet :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet r f = let pr = delete empty $ powerset r
                   step1 = closeRef pr f
               in closeSet' pr step1

closeSet' :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet' pr f = let step2 = closeAum pr f
                     step3 = closeTrans step2
                in if step3 == f then step3 else closeSet' pr step3

closeSetPrint r f = niceShow $ closeSet r f

closeSetCard r f = size $ closeSet r f

-- Ejercicio 2

-- closeAlfa :: Set String -> Set (Set String, Set String) -> Set String
closeAlfa alfa f = let aux = Data.Set.filter (\(x,y)-> x `isSubsetOf` alfa) f
                       aux1 = unions $ toList $ Data.Set.map snd aux
                       aux2 = union aux1 alfa
                   in if aux2 == alfa then aux2 else closeAlfa aux2 f











------------------------------------------------
