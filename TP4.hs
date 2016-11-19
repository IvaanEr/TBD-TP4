module TP4 where 

import Data.Set
import Control.Monad

-- para mostrar los conjuntos de forma mas prolija quitandole los fromList
niceShow :: Set (Set String,Set String) -> [(String,String)]
niceShow f = toList  $ Data.Set.map (\(x,y) -> (concat $ toList  x,concat $ toList  y)) f

-- para ingresar conjuntos R o alfas de forma menos tediosa.
entryR :: String -> Set String
entryR xs = Prelude.foldr (\x -> union $ fromList [[x]]) empty xs

-- idem entryR pero para conjunto de DF.
entryF :: [(String,String)] -> Set (Set String,Set String)
entryF xs = fromList (Prelude.map (\(x,y) -> (entryR x, entryR y)) xs )

-- conjunto de partes
powerset :: Ord a => Set a -> Set (Set a)
powerset = fromList . fmap fromList . listPowerset . toList
 
listPowerset :: [a] -> [[a]]
listPowerset = filterM (const [True, False])

-- regla de reflexividad a un conjunto "alfa"
reflex :: Set String -> Set (Set String, Set String)
reflex set | set == empty = empty
           | otherwise = let part = delete empty $ powerset set 
                         in Data.Set.map (\x -> (set,x)) part 

-- regla de reflexividad a P(R)
closeRef :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeRef pr f = let aux = unions $ toList $ Data.Set.map reflex pr
                in union f aux

-- regla de aumento a P(R) y un conjunto de DF.
closeAum :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeAum pr f = unions $ toList $ Data.Set.map (\rr -> Data.Set.map (\(x,y)-> (union x rr, union y rr)) f ) pr

-- regla de transitividad
closeTrans :: Set (Set String,Set String) -> Set (Set String,Set String)
closeTrans f = unions $ toList $ Data.Set.map (\(a1,b1)-> Data.Set.map (\(a2,b2)-> if b1 == a2 then (a1,b2) else (a2,b2)) f) f


-- F+ !!!
closeSet :: Set String -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet r f = let pr = delete empty $ powerset r
                   step1 = closeRef pr f
               in closeSet' pr step1

closeSet' :: Set (Set String) -> Set (Set String,Set String) -> Set (Set String,Set String)
closeSet' pr f = let step2 = closeAum pr f
                     step3 = closeTrans step2
                in if step3 == f then step3 else closeSet' pr step3

-- dado R y F calcula F+ y lo imprime
closeSetPrint r f = niceShow $ closeSet r f

-- dado R y F calcula F+ y calcula su cardinalidad
closeSetCard r f = size $ closeSet r f

-- Ejercicio 2

closeAlfa :: Set String -> Set (Set String, Set String) -> Set String
closeAlfa alfa f = let aux = Data.Set.filter (\(x,y)-> x `isSubsetOf` alfa) f -- para cada df. nos quedamos con las que su lado derecho esta en alfa
                       aux1 = unions $ toList $ Data.Set.map snd aux -- juntamos todos los lados derechos de las df. que satisfacen.
                       aux2 = union aux1 alfa 
                   in if aux2 == alfa then aux2 else closeAlfa aux2 f -- verificamos si terminamos

-- simplemente para imprimirlo lindo con las comas, asqueroso... pero funciona.
printSet :: Set String -> IO ()
printSet s = do putStr "{ "
                let xs = toList s
                    ys = concat $ ((Prelude.map (\x -> x++", ") (init xs)) ++ [(last xs)])
                putStr ys 
                putStr " }"
                putStrLn ""

-- Ejercicio 3

-- Ejercicio 4

-- candidateKey :: Set String -> Set (Set String -> Set String) -> Set (Set String)
candidateKey r f = let pr = delete empty $ powerset r
                   in Data.Set.map (\x -> if closeAlfa x f == r then x else empty) pr 


claves :: Set String -> Set (Set String, Set String) -> Set (Set String)
claves r f = claves' r empty f

-- claves' :: Set String -> Set (Set String) ->  Set (Set String, Set String) -> Set (Set String)
claves' r res f = let pr = delete empty $ powerset r
                      alfas = Data.Set.filter (\x -> closeAlfa x f == r) pr --quedan los "alfas" que son claves
                  in  Data.Set.filter (\alfa-> Data.Set.map (\x-> ) res) alfas