{- 
 - PARTICIPANTES : Marcos Rocha Morales y Marina Bueno García
 - ASIGNATURA    : PRDE(HASKELL)
 - PROYECTO      : Solucionador de sudokus utilizando los siguientes métodos:

  - Update: actualiza la lista en Nota [Int] de los números posibles en una casilla
  Definitivo 3  Nota [1,2,3,4,5,6,7,8,9]  Definitivo 6  Definitivo 9  Definitivo 7  Definitivo 8  Nota [1,2,3,4,5,6,7,8,9]  Nota [1,2,3,4,5,6,7,8,9]  Nota [1,2,3,4,5,6,7,8,9]
  Definitivo 3  Nota [1,2,4,5]  Definitivo 6  Definitivo 9  Definitivo 7  Definitivo 8  Nota [1,2,4,5]   Nota [1,2,4,5]  Nota [1,2,4,5] 

  - Naked candidates: convierte en Definitivo Int aquellas celdas que sean Nota [Int] donde la lista solo contiene un elemento
    Definitivo 3  Nota [1]  Definitivo 6  Definitivo 9  Definitivo 7  Definitivo 8  Nota [1,2,4,5]   Nota [1,2,4,5]  Nota [1,2,4,5] 
    Definitivo 3  Definitivo 1  Definitivo 6  Definitivo 9  Definitivo 7  Definitivo 8  Nota [1,2,4,5]   Nota [1,2,4,5]  Nota [1,2,4,5] 

  - Hiden singles: compara las listas (Nota [Int]) y convierte en definitivo los números que aparecen una sola vez en todas las listas
    Supongamos que tenemos la siguiente fila:
          Definitivo 3   Nota [1,2,4,5]   Definitivo 6   Definitivo 9   Definitivo 7   Definitivo 8   Nota [2,4]   Nota [2,5]   Nota [2,4,5]
    Como el 1 aparece en todas las Nota [Int] una sola vez se puede convertir en definitivo:
          Definitivo 3   Definitivo 1   Definitivo 6   Definitivo 9   Definitivo 7   Definitivo 8   Nota [2,4]   Nota [2,5]   Nota [2,4,5]

  - Pairs: consiste en encontrar parejas que aparecen en 2 listas (Nota [Int]) y eliminar el resto de elementos que no sean esa pareja,
    consiguiendo así acotar los números posibles en una celda
      Definitivo 3   Nota [2,4,5]   Definitivo 6   Definitivo 9   Definitivo 7   Definitivo 8   Nota [1,4]   Nota [1,5]   Nota [1,4,5]
      Definitivo 3   Nota [4,5]   Definitivo 6   Definitivo 9   Definitivo 7   Definitivo 8   Nota [1,4]   Nota [1,5]   Nota [4,5]

  - Pointing pairs 
 -}

{- REVISAR
- deriving
- cambiar procesa por un main
- quitar provisional de Celda
-}

import Data.Char
import Data.List

data Celda = Definitivo Int |Provisional Int | Nota [Int]
     deriving (Show, Read, Eq)
type Fila = [Celda]
type Sudoku = [Fila]

procesa :: IO ()
procesa = do putStr "Dime el nombre del sudoku: "
             name <- getLine
             content <- readFile name
             let
                line = map init (lines content)
                sudoku = toSudoku line
                solved = solveSudoku sudoku
             print(line)
             putStr $ showSudoku solved
             putStr $ showSudoku sudoku
             writeFile "result.txt" (showSudoku solved)

-- Función que aplica todos los métodos implementados (step) hasta resolverlo (solved)
solveSudoku :: Sudoku -> Sudoku
solveSudoku xs
            | and(map solved xs) = xs
            | otherwise = solveSudoku (step xs)

solved :: Fila -> Bool
solved [] = True
solved (Definitivo x:xs) = True && solved xs
solved (_:xs) = False

step :: Sudoku -> Sudoku
step xs = pairs (update (hidenSingles (update (nakedCandidates (update xs)))))

--Devuelve las lineas de un String
--readSudoku :: String -> [String]
--readSudoku xs = lines xs

-- Convertimos la lista de string leida de un archivo en nuestro tipo Sudoku
toSudoku :: [String] -> Sudoku
toSudoku xs = zipWith toFila xs xs

toFila :: String -> String -> Fila
toFila [] _ = []
toFila (x:xs) ys
         | Data.Char.isDigit x = [Definitivo (Data.Char.digitToInt x)] ++ toFila xs ys
         | otherwise = [Nota [1..9]] ++ toFila xs ys




-----------------------  UPDATE   ---------------------------
-- Actualiza Nota [Int] teniendo en cuenta filas, columnas y cuadrículas (3x3)
update :: Sudoku -> Sudoku
update xs = (updateNotaFila (updateNotaColumna (juntarGrid (updateNotaGrid 0 xs))))


----------- UPDATE FILA
--Actualiza Nota [Int] de todo el sudoku mirando la fila
updateNotaFila :: Sudoku -> Sudoku
updateNotaFila [] = []
updateNotaFila (x:xs) = [updateFila lista x] ++ updateNotaFila xs
               where lista = inRow x

--Devuelve una lista con los números Definitivos en una fila
inRow :: Fila -> [Int]
inRow [] = []
inRow ((Definitivo x):xs)  =  [x] ++ inRow xs
inRow ((Nota ys):xs) = inRow xs

-- Actualiza Nota [Int] de una fila
updateFila :: [Int] -> Fila -> Fila
updateFila xs ((Definitivo e):es)
                         | es /= [] = [Definitivo e] ++ updateFila xs es
                         | otherwise = [Definitivo e]
updateFila xs ((Nota ys):es)
                         | es /= [] = [Nota (eliminaElems xs ys)] ++ updateFila xs es
                         | otherwise = [Nota (eliminaElems xs ys)]

--Elimina todos los elementos de una lista de otra lista (elimina la intersección de dos listas)
eliminaElems :: [Int] -> [Int] -> [Int]
eliminaElems [] ys = ys
eliminaElems (x:xs) ys
        | x `elem` ys = eliminaElems xs (Data.List.delete x ys)
        | otherwise = eliminaElems xs ys


----------- UPDATE COLUMNA
--Actualiza Nota [Int] de todo el sudoku mirando la columna
updateNotaColumna :: Sudoku -> Sudoku
updateNotaColumna xs = Data.List.transpose (updateNotaFila (Data.List.transpose xs))


----------- UPDATE GRID
--Actualiza Nota [Int] de todo el sudoku mirando las cuadrículas
updateNotaGrid :: Int -> Sudoku -> Sudoku
updateNotaGrid 27 _ = []
updateNotaGrid a xs = updateGrid ys ys ++ updateNotaGrid (a+3) xs
                      where ys = subGridToRow a 1 xs

-- Actualiza Nota [Int] de una cuadrícula
updateGrid :: Sudoku -> Sudoku -> Sudoku
updateGrid [] _ = []
updateGrid (x:xs) ys = [updateFila lista x] ++ updateGrid xs ys
                   where lista = concat (map inRow ys)

-- Convierte una cuadrícula en un Sudoku con una fila
subGridToRow:: Int -> Int -> Sudoku -> Sudoku
subGridToRow a 4 _ = []
subGridToRow a b xs = [take 3 (drop (a `mod` 9) (ys !! (b-1)))] ++ subGridToRow a (b+1) xs
                         where ys = drop ((a`div`9)*3) xs

-- Función que une las cuadrículas de vuelta al Sudoku original
juntarGrid :: Sudoku -> Sudoku 
juntarGrid [] = []
juntarGrid xs = [foldl (++) [] (juntarGrid2 i ys) | i <- [0,1,2]] ++ juntarGrid (drop 9 xs)
                    where ys = take 9 xs

juntarGrid2 :: Int -> Sudoku -> Sudoku
juntarGrid2 a xs 
          | a >= length xs = []
          | otherwise = [(xs !! a)] ++ juntarGrid2 (a+3) xs




-----------------------   FILL SUDOKU   ---------------------------

----------- MÉTODO 1: NAKED SUDOKUS: convierte en definitivo el único elemento de la lista Nota [Int] (lista con un único elemento)
nakedCandidates :: Sudoku -> Sudoku
nakedCandidates xs = map notaToDefinitivo xs

notaToDefinitivo:: Fila -> Fila
notaToDefinitivo [] = []
notaToDefinitivo ((Definitivo x):xs) = [Definitivo x] ++ notaToDefinitivo xs
notaToDefinitivo ((Nota x):xs)
              | length x == 1 = [Definitivo (x !! 0)] ++ notaToDefinitivo xs
              | otherwise = [Nota x] ++ notaToDefinitivo xs




----------- MÉTODO 2: HIDEN SINGLES: compara las listas (Nota [Int]) de filas, columnas y cuadrículas convirtiendo en definitivo
----------- aquellos elementos que solo están en una de las listas
hidenSingles :: Sudoku -> Sudoku
hidenSingles xs = update (hidenSinglesFila (update (hidenSinglesColumna (update (juntarGrid (hidenSinglesGrid 0 xs))))))

----------- MÉTODO 2: HIDEN SINGLES FILA

hidenSinglesFila :: Sudoku -> Sudoku
hidenSinglesFila [] = []
hidenSinglesFila (x:xs) = [hidenSinglesFilaLista lista x] ++ hidenSinglesFila xs
                   where lista = delRep 0 (concatNota x)

-- Comprueba si una lista de Int (que aparecen una sola vez) verifica hidenSinglesFilaElem
hidenSinglesFilaLista :: [Int] -> Fila -> Fila
hidenSinglesFilaLista [] xs = xs
hidenSinglesFilaLista (a:as) xs = hidenSinglesFilaLista as (hidenSinglesFilaElem a xs)

-- Comprueba si un elemento (que aparece 1 vez) está en Nota [Int] y lo convierte en Definitivo 
hidenSinglesFilaElem :: Int -> Fila -> Fila
hidenSinglesFilaElem a [] = []
hidenSinglesFilaElem a ((Nota x):xs)
               | a `elem` x = [Definitivo a] ++ xs   
               | otherwise = [Nota x] ++ hidenSinglesFilaElem a xs
hidenSinglesFilaElem a ((Definitivo x):xs) = [Definitivo x] ++ hidenSinglesFilaElem a xs

-- Concatena las lista de Nota [Int] de una fila
concatNota :: Fila -> [Int]
concatNota [] = []
concatNota ((Definitivo x):xs) = concatNota xs
concatNota ((Nota x):xs) = x ++ concatNota xs

-- elimina todos los elementos que aparecen i+1 veces en una lista
delRep :: Int -> [Int] -> [Int]
delRep _ [] = []
delRep i (x:xs)
          | numRep x xs /= i = delRep i (delAllRep x xs)
          | otherwise = [x] ++ delRep i xs

-- devuelve el número de veces que se repite un número en una lista
numRep :: Int -> [Int] -> Int
numRep n [] = 0
numRep n (x:xs) 
            | x == n = 1 + numRep n xs
            | otherwise = numRep n xs

-- elimina todas las repeticiones de un numero
delAllRep :: Int -> [Int] -> [Int]
delAllRep x [] = []
delAllRep x (y:ys)
          | x == y = delAllRep x ys
          | otherwise = [y] ++ delAllRep x ys


----------- MÉTODO 2: HIDEN SINGLES COLUMNA

hidenSinglesColumna :: Sudoku -> Sudoku
hidenSinglesColumna xs = Data.List.transpose (hidenSinglesFila (Data.List.transpose xs))


----------- MÉTODO 2: HIDEN SINGLES CUADRÍCULA

hidenSinglesGrid :: Int -> Sudoku -> Sudoku
hidenSinglesGrid 27 _ = []
hidenSinglesGrid a xs = hidenSinglesSubgrid ys ++ hidenSinglesGrid (a+3) xs
                      where ys = subGridToRow a 1 xs

hidenSinglesSubgrid :: Sudoku -> Sudoku
hidenSinglesSubgrid xs = hidenSinglesSubgridLista lista xs
                  where lista = delRep 0 (concat (map concatNota xs))

hidenSinglesSubgridLista :: [Int] -> Sudoku -> Sudoku
hidenSinglesSubgridLista as [] = []
hidenSinglesSubgridLista as (x:xs) = [hidenSinglesFilaLista as x] ++ hidenSinglesSubgridLista as xs




----------- MÉTODO 3: PAIRS: encontrar parejas en Nota [Int] para acotar soluciones

pairs :: Sudoku -> Sudoku
pairs xs = pairsFila (pairsColum (juntarGrid (pairsGrid 0 xs)))
----------- MÉTODO 3: PAIRS FILA

pairsFila :: Sudoku -> Sudoku
pairsFila [] = []
pairsFila (x:xs) = [updatePairs c x] ++ pairsFila xs
                   where lista = delRep 1 (concatNota x)
                         c = [[x,y] | x<-lista, y<-lista, x < y]

-- Si cada pareja aparece 2 veces entonces actualizamos los Nota [Int] que contengan estas parejas
updatePairs :: [[Int]] -> Fila -> Fila 
updatePairs [] xs = xs
updatePairs (c:cs) xs
             | countPairs c xs == 2 = updatePairPerPair c xs
             | otherwise = updatePairs cs xs

-- Cuenta cuantas listas (Notas [Int]) contienen a la pareja dada
countPairs :: [Int] -> Fila -> Int
countPairs as [] = 0
countPairs as (Nota x:xs)
            | Data.List.isInfixOf as x = 1 + countPairs as xs
            | otherwise = countPairs as xs
countPairs as (Definitivo x:xs) = countPairs as xs

-- Dejamos Nota [Int] solo con los elementos que forman la pareja, eliminando el resto
updatePairPerPair :: [Int] -> Fila -> Fila
updatePairPerPair [] xs = xs 
updatePairPerPair _ [] = []
updatePairPerPair as (Nota x:xs)
            |Data.List.isInfixOf as x = [Nota as] ++ updatePairPerPair as xs
            |otherwise = [Nota x] ++ updatePairPerPair as xs
updatePairPerPair as (Definitivo x:xs) = [Definitivo x] ++ updatePairPerPair as xs


----------- MÉTODO 3: PAIRS COLUMNA

pairsColum :: Sudoku -> Sudoku
pairsColum xs = Data.List.transpose (pairsFila (Data.List.transpose xs))


----------- MÉTODO 3: PAIRS GRID

pairsGrid :: Int -> Sudoku -> Sudoku
pairsGrid 27 _ = []
pairsGrid a xs = pairsSubgrid ys ++ pairsGrid (a+3) xs
                      where ys = subGridToRow a 1 xs

pairsSubgrid :: Sudoku -> Sudoku
pairsSubgrid xs = pairsSubgridLista lista xs
                  where lista = delRep 1 (concat (map concatNota xs))

pairsSubgridLista :: [Int] -> Sudoku -> Sudoku
pairsSubgridLista as [] = []
pairsSubgridLista as (x:xs) = [updatePairs c x] ++ pairsSubgridLista as xs
                        where c = [[x,y] | x<-as, y<-as, x < y]




----------- MÉTODO 4: POINTING PAIRS: encontrar parejas en Nota [Int] que apunten a una celda que contiene uno de los elementos de la pareja

pointingPairs :: Sudoku -> Sudoku
pointingPairs [] = []
pointingPairs (x:xs) = [updatePointingPairs pairs x] ++ pointingPairs xs
                        where pairs = getPairs x x

getPairs :: Fila -> Fila -> [[Int]]
getPairs [] ys = []
getPairs (Nota x:xs) ys
              | length x == 2 && isThisPair x ys == 2 = [x]++getPairs xs ys
              | otherwise = getPairs xs ys
getPairs (Definitivo x:xs) ys = getPairs xs ys

isThisPair :: [Int] -> Fila -> Int
isThisPair as [] = 0
isThisPair as (Nota x:xs)
            | as == x = 1 + isThisPair as xs
            | otherwise = isThisPair as xs
isThisPair as (Definitivo x:xs) = isThisPair as xs

updatePointingPairs :: [[Int]] -> Fila -> Fila
updatePointingPairs [] xs = xs
updatePointingPairs (a:as) xs = updatePointingPairs as (updateOnePointingPair a xs)

updateOnePointingPair :: [Int] -> Fila -> Fila
updateOnePointingPair as (Nota x:xs) = [Nota [ y | y <- x, y `notElem` as]]



-----------------------   SHOW   ---------------------------

-- Muestra los DefinitIvo Int del Sudoku
showSudoku :: Sudoku -> String
showSudoku = unlines . map (unwords . map showCelda)
  where
    showCelda (Definitivo x) = show x
    showCelda _ = "."

--Muestra todas las celdas del Sudoku
showPosSudoku :: Sudoku -> String
showPosSudoku = unlines . map (unwords . map showCelda)
  where
    showCelda (Provisional x) = show x
    showCelda (Nota xs) = show xs
    showCelda (Definitivo x) = show x