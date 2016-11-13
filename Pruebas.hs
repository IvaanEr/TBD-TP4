module Pruebas where

import Data.Set

r' = fromList ["A","B","C","D"]
f' = fromList [(fromList ["A"],fromList ["B"]),(fromList ["B"],fromList ["C"]),(fromList ["C"],fromList ["D"])]

-- Ejercicio 1
r1 = fromList ["A","B","C","D"]
f1 = fromList [(fromList ["A"],fromList ["B"]),(fromList ["C","B"],fromList ["A"]),(fromList ["B"],fromList["A","D"])]

r2 = fromList ["A","B","C","D","E", "F"]
f2 = fromList [(fromList ["A","B"],fromList ["C"]),(fromList ["B","D"],fromList ["E","F"])]
