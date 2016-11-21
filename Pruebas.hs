--module Pruebas where

import Data.Set
import Control.Monad
import TP4 

-- r' = fromList ["A","B","C","D"]
-- f' = fromList [(fromList ["A"],fromList ["B"]),(fromList ["B"],fromList ["C"]),(fromList ["C"],fromList ["D"])]

-- Ejercicio 1
r1 = fromList ["A","B","C","D"]
f1 = fromList [(fromList ["A"],fromList ["B"]),(fromList ["C","B"],fromList ["A"]),(fromList ["B"],fromList["A","D"])]

r2 = fromList ["A","B","C","D","E", "F"]
f2 = fromList [(fromList ["A","B"],fromList ["C"]),(fromList ["B","D"],fromList ["E","F"])]

-- Ejercicio 2

r3 = entryR "ABCDEFGHIJ"
f3 = entryF [("A","I"),("AB","C"),("AD","GH"),("BD","EF"),("H","J")]
alfa3 = entryR "BD"

r4 = entryR "ABCDEFGH"
f4 = entryF [("A","BC"),("C","D"),("D","G"),("E","A"),("E","H"),("H","E")]
alfa4 = entryR "AC"

r5 = entryR "ABCDEFG"
f5 = entryF [("A","F"),("A","G"),("B","E"),("C","D"),("D","B"),("E","A"),("FG","C")]
alfa5 = entryR "FG"

prueba1 :: IO ()
prueba1 = do putStrLn "Set1 -> R: {A, B, C, D} F: {{A}→{B}, {B}→{A,D}, {B,C}→{A}}"
             putStr "Cardinalidad:"
             putStrLn $ show $ closeSetCard r1 f1
             putStrLn "Set2 -> R: {A, B, C, D, E, F} F: {AB→C, BD→EF}"
             putStr "Cardinalidad:"
             putStrLn $ show $ closeSetCard r2 f2
             
prueba2 :: IO()
prueba2 = do putStrLn "Set3 -> R: { A, B, C, D, E, F, G, H, I, J } F: {{A}→{I}, {A,B}→{C}, {A,D}→{G,H}, {B,D}→{E,F}, {H}→{J}}"
             putStrLn "A: { B, D }"
             putStr "A+: " 
             printSet $ closeAlfa alfa3 f3
             putStrLn "Set4 -> R: {A,B,C,D,E,F,G,H} F: {{A→BC},{C→D},{D→G},{E→A},{E→H},{H→E}}"
             putStrLn "A: {A,C}"
             putStr "A+: "
             printSet $ closeAlfa alfa4 f4
             putStrLn "Set5 -> R: {A,B,C,D,E,F,G} F: {{A→F},{A→G},{B→E},{C→D},{D→B},{E→A}, {FG→C}}"
             putStrLn "A: {F,G}"
             putStr "A+: "
             printSet $ closeAlfa alfa5 f5

prueba3 :: IO()
prueba3 = do putStrLn "Set3 -> R: { A, B, C, D, E, F, G, H, I, J } F: {{A}→{I}, {A,B}→{C}, {A,D}→{G,H}, {B,D}→{E,F}, {H}→{J}}"
             putStrLn "Claves candidatas: " 
             printSet' $ toList $ claves r3 f3
             putStrLn "Set4 -> R: {A,B,C,D,E,F,G,H} F: {{A→BC},{C→D},{D→G},{E→A},{E→H},{H→E}}"
             putStrLn "Claves candidatas: "
             printSet' $ toList $ claves r4 f4
             putStrLn "Set5 -> R: {A,B,C,D,E,F,G} F: {{A→F},{A→G},{B→E},{C→D},{D→B},{E→A}, {FG→C}}"
             putStrLn "Claves candidatas: "
             printSet' $ toList $ claves r5 f5

main :: IO()
main = do prueba1
          prueba2 
          prueba3