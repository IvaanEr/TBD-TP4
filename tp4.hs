-- module TP4 where 

import Data.Set
import Control.Monad

-- showwwwww
niceShow f = toList  $ Data.Set.map (\(x,y) -> (concat $ toList  x,concat $ toList  y)) f

r1 = fromList ["A","B","C","D"]
f1 = fromList [(fromList ["A"],fromList ["B"]),(fromList ["C","B"],fromList ["A"]),(fromList ["B"],fromList["A","D"])]

r2 = fromList ["A","B","C","D","E", "F"]
f2 = fromList [(fromList ["A","B"],fromList ["C"]),(fromList ["B","D"],fromList ["E","F"])]

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
--alfa -> beta => alfa gama => beta gama tq gama \in 

closeAum :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeAum r f = let pr = delete empty $ powerset r                       
               in unions $ toList $ Data.Set.map (\rr -> Data.Set.map (\(x,y)-> (union x rr, union y rr)) f ) pr

f' = fromList [(fromList ["A"],fromList ["B"]),(fromList ["B"],fromList ["C"]),(fromList ["C"],fromList ["D"])]

closeTrans :: Set (Set String,Set String) -> Set (Set String,Set String)
closeTrans f = unions $ toList $ Data.Set.map (\(a1,b1)-> Data.Set.map (\(a2,b2)-> if b1 == a2 then (a1,b2) else (a2,b2)) f) f


closeSet :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet r f = let step1 = closeRef r f
               in closeSet' r step1

closeSet' :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet' r f = let step2 = closeAum r f
                    step3 = closeTrans step2
                in if step3 == f then step3 else closeSet' r step3


closeSetPrint r f = toList  $ Data.Set.map (\(x,y) -> (concat $ toList  x,concat $ toList  y)) $ closeSet r f

closeSetCard r f = size $  closeSet r f

prueba :: IO ()
prueba = do putStrLn "Set1 -> R: {A, B, C, D} F: {{A}→{B}, {B}→{A,D}, {B,C}→{A}}"
            putStr "Cardinalidad:"
            putStrLn $ show $ closeSetCard r1 f1
            putStrLn "Set2 -> R: {A, B, C, D, E, F} F: {AB→C, BD→EF}"
            putStr "Cardinalidad:"
            putStrLn $ show $ closeSetCard r2 f2

