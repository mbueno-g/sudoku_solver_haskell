{- PROYECTO      :Solucionador de sudokus
 - PARTICIPANTES : Marcos Rocha Morales y Marina Bueno García
 - ASIGNATURA    : PRDE(HASKELL)
 -}

import Data.Char
import Data.List

data Celda = Definitivo Int | Provisional Int | Nota [Int]
     deriving (Show, Read, Eq)
type Fila = [Celda]
type Sudoku = [Fila]


-----------------------  READ   ---------------------------

procesa :: IO ()
procesa = do putStr "Dime el nombre del sudoku: "
             name <- getLine
             content <- readFile name
             let
                line = map init (readSudoku content)
                sudoku = toSudoku line
                solved = solveSudoku sudoku
             print(line)
             putStr $ showSudoku solved
             putStr $ showSudoku sudoku
             writeFile "result.txt" (showSudoku solved)

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
readSudoku :: String -> [String]
readSudoku xs = lines xs


--Crea el tipo Sudoku
toSudoku :: [String] -> Sudoku
toSudoku xs = zipWith toFila xs xs

{--
toFila :: String -> String -> Fila
toFila [] _ = []
toFila (x:xs) ys
         | Data.Char.isDigit x && x >= '1' && x <= Data.Char.intToDigit (length ys) = [Definitivo (Data.Char.digitToInt x)] ++ toFila xs ys
         | otherwise = [Nota [1..n]] ++ toFila xs ys
              where n = length ys

--}

toFila :: String -> String -> Fila
toFila [] _ = []
toFila (x:xs) ys
         | Data.Char.isDigit x = [Definitivo (Data.Char.digitToInt x)] ++ toFila xs ys
         | otherwise = [Nota [1..9]] ++ toFila xs ys


-----------------------  UPDATE   ---------------------------
-- Lo actualiza TODO
update :: Sudoku -> Sudoku
update xs = (updateNotaf (updateNotac (juntarGrid (updateNotaGs 0 xs))))

--Elementos definitivos de una Fila
inRow :: Fila -> [Int]
inRow [] = []
inRow ((Definitivo x):xs)  =  [x] ++ inRow xs
inRow ((Nota ys):xs) = inRow xs


--Actualiza Nota [Int] mirando la fila
updateNotaf :: Sudoku -> Sudoku
updateNotaf [] = []
updateNotaf (x:xs) = [updateFila lista x] ++ updateNotaf xs
               where lista = inRow x


-- Actualiza Nota [Int] mirando la columna
updateNotac :: Sudoku -> Sudoku
updateNotac xs = Data.List.transpose (updateNotaf (Data.List.transpose xs))


-- Actualiza Nota [Int] mirando las cuadrículas

{--juntarGrid :: Sudoku -> Sudoku 
juntarGrid [] = []
juntarGrid xs = [foldl (++) [] (juntar3 0 ys),foldl (++) [] (juntar3 1 ys),foldl (++) [] (juntar3 2 ys)] ++ juntarGrid (drop 9 xs)
                    where ys = take 9 xs
-}

juntarGrid :: Sudoku -> Sudoku 
juntarGrid [] = []
juntarGrid xs = [foldl (++) [] (juntar3 i ys) | i <- [0,1,2]] ++ juntarGrid (drop 9 xs)
                    where ys = take 9 xs

juntar3 :: Int -> Sudoku -> Sudoku
juntar3 a xs 
          | a >= length xs = []
          | otherwise = [(xs !! a)] ++ juntar3 (a+3) xs

updateNotaGs :: Int -> Sudoku -> Sudoku
updateNotaGs 27 _ = []
updateNotaGs a xs = updateNotag ys ys ++ updateNotaGs (a+3) xs
                      where ys = subGridToRow a 1 xs

updateNotag :: Sudoku -> Sudoku -> Sudoku
updateNotag [] _ = []
updateNotag (x:xs) ys = [updateFila lista x] ++ updateNotag xs ys
                   where lista = concat (map inRow ys)

subGridToRow:: Int -> Int -> Sudoku -> Sudoku
subGridToRow a 4 _ = []
subGridToRow a b xs = [take 3 (drop (a `mod` 9) (ys !! (b-1)))] ++ subGridToRow a (b+1) xs
                         where ys = drop ((a`div`9)*3) xs


-- Actualiza Nota [Int] mirando la fila
updateFila :: [Int] -> Fila -> Fila
updateFila xs ((Definitivo e):es)
                         | es /= [] = [Definitivo e] ++ updateFila xs es
                         | otherwise = [Definitivo e]
updateFila xs ((Nota ys):es)
                         | es /= [] = [Nota (eliminaElems xs ys)] ++ updateFila xs es
                         | otherwise = [Nota (eliminaElems xs ys)]

--Elimina todos los elementos de una lista de otra lista
eliminaElems :: [Int] -> [Int] -> [Int]
eliminaElems [] ys = ys
eliminaElems (x:xs) ys
        | x `elem` ys = eliminaElems xs (Data.List.delete x ys)
        | otherwise = eliminaElems xs ys



-----------------------   FILL SUDOKU   ---------------------------

---------------------------------------------------------------------------------------------------------------------------------------- MÉTODO 1
-- Convierte en definitivo el único elemento de la lista Nota [Int] (lista con un único elemento)
nakedCandidates :: Sudoku -> Sudoku
nakedCandidates xs = map notaToDefinitivo xs

notaToDefinitivo:: Fila -> Fila
notaToDefinitivo [] = []
notaToDefinitivo ((Definitivo x):xs) = [Definitivo x] ++ notaToDefinitivo xs
notaToDefinitivo ((Nota x):xs)
              | length x == 1 = [Definitivo (x !! 0)] ++ notaToDefinitivo xs
              | otherwise = [Nota x] ++ notaToDefinitivo xs
---------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------MÉTODO 2 FILA

-- Compara las listas (Nota [Int]) de toda una fila y convierte en definitivo el elemento que solo está en una de esas lista
-- Actualiza la fila en función de la lista resultante de concatenar todos los Nota [Int] y eliminar los repetidos
metodo2Fila :: Sudoku -> Sudoku
metodo2Fila [] = []
metodo2Fila (x:xs) = [updateFilaLista lista x] ++ metodo2Fila xs
                   where lista = delRep 0 (concatNota x)

-- Comprueba si una lista de Int (que aparecen una sola vez) verifica updateFilaElem
updateFilaLista :: [Int] -> Fila -> Fila
updateFilaLista [] xs = xs
updateFilaLista (a:as) xs = updateFilaLista as (updateFilaElem a xs)

-- Comprueba si un elemento (aparece una sola vez) está en Nota [Int] y lo convierte en Definitivo 
updateFilaElem :: Int -> Fila -> Fila
updateFilaElem a [] = []
updateFilaElem a ((Nota x):xs)
               | a `elem` x = [Definitivo a] ++ xs   
               | otherwise = [Nota x] ++ updateFilaElem a xs
updateFilaElem a ((Definitivo x):xs) = [Definitivo x] ++ updateFilaElem a xs
--PARA ACTUALIZAR EL SUDOKU TRAS PONER UN DEFINITIVO HAY QUE LLAMAR A UPDATE

-- Concatena las lista de Nota [Int]
concatNota :: Fila -> [Int]
concatNota [] = []
concatNota ((Definitivo x):xs) = concatNota xs
concatNota ((Nota x):xs) = x ++ concatNota xs

-- elimina todos los elementos repetidos de una lista dejando solo los que aparecen una vez
delRep :: Int -> [Int] -> [Int]
delRep _ [] = []
delRep i (x:xs)
          | numRep x xs /= i = delRep  i (delAllRep x xs)
          | otherwise = [x] ++ delRep i xs


-- devuelve el número de repeticiones de un número
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
------------------------------------------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------------------------------------ MÉTODO 2 COLUMNA
--Compara las listas (Nota [Int]) de toda una columna y convierte en definitivo el elemento que solo está en una de esas listas

metodo2Colum :: Sudoku -> Sudoku
metodo2Colum xs = Data.List.transpose (metodo2Fila (Data.List.transpose xs))

-------------------------------------------------------------------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------------------------------------------------- MÉTODO 2 GRID 
--Compara las listas (Nota [Int]) de toda una columna y convierte en definitivo el elemento que solo está en una de esas listas

metodo2Grid :: Int -> Sudoku -> Sudoku
metodo2Grid 27 _ = []
metodo2Grid a xs = updateGrid2 ys ++ metodo2Grid (a+3) xs
                      where ys = subGridToRow a 1 xs

updateGrid2 :: Sudoku -> Sudoku
updateGrid2 xs = metodo2Filax lista xs
                  where lista = delRep 0 (concat (map concatNota xs))

metodo2Filax :: [Int] -> Sudoku -> Sudoku
metodo2Filax as [] = []
metodo2Filax as (x:xs) = [updateFilaLista as x] ++ metodo2Filax as xs

hidenSingles :: Sudoku -> Sudoku
hidenSingles xs = update (metodo2Fila (update (metodo2Colum (update (juntarGrid (metodo2Grid 0 xs))))))


-----------------------------------------------------------------------------------------------------------------------------------
---MÉTODO 3: Encontrar parejas en Nota [Int] para acotar soluciones
---------------------------------------------------------------------------------------------------------------------

pairsFila :: Sudoku -> Sudoku
pairsFila [] = []
pairsFila (x:xs) = [updatePairs c x] ++ pairsFila xs
                   where lista = delRep 1 (concatNota x)
                         c = [[x,y] | x<-lista, y<-lista, x < y]

updatePairs :: [[Int]] -> Fila -> Fila 
updatePairs [] xs = xs
updatePairs (c:cs) xs
             | countPairs c xs == 2 = updatePairPerPair c xs
             | otherwise = updatePairs cs xs

countPairs :: [Int] -> Fila -> Int
countPairs as [] = 0
countPairs as (Nota x:xs)
            | Data.List.isInfixOf as x = 1 + countPairs as xs
            | otherwise = countPairs as xs
countPairs as (Definitivo x:xs) = countPairs as xs

updatePairPerPair :: [Int] -> Fila -> Fila
updatePairPerPair [] xs = xs 
updatePairPerPair _ [] = []
updatePairPerPair as (Nota x:xs)
            |Data.List.isInfixOf as x = [Nota as] ++ updatePairPerPair as xs
            |otherwise = [Nota x] ++ updatePairPerPair as xs
updatePairPerPair as (Definitivo x:xs) = [Definitivo x] ++ updatePairPerPair as xs

pairsColum :: Sudoku -> Sudoku
pairsColum xs = Data.List.transpose (pairsFila (Data.List.transpose xs))

pairsGrid :: Int -> Sudoku -> Sudoku
pairsGrid 27 _ = []
pairsGrid a xs = updatePairsGrid ys ++ pairsGrid (a+3) xs
                      where ys = subGridToRow a 1 xs

updatePairsGrid :: Sudoku -> Sudoku
updatePairsGrid xs = updatePairsFilax lista xs
                  where lista = delRep 1 (concat (map concatNota xs))

updatePairsFilax :: [Int] -> Sudoku -> Sudoku
updatePairsFilax as [] = []
updatePairsFilax as (x:xs) = [updatePairs c x] ++ updatePairsFilax as xs
                        where c = [[x,y] | x<-as, y<-as, x < y]


pairs :: Sudoku -> Sudoku
pairs xs = pairsFila (pairsColum (juntarGrid (pairsGrid 0 xs)))


-----------------------------------------------------------------------------------------------------------------------------------
---MÉTODO 4: Encontrar parejas en Nota [Int] que apunten a una celda que contiene uno de los elementos de la pareja
---------------------------------------------------------------------------------------------------------------------

pointingPairs :: Sudoku -> Sudoku
pointingPairs [] = []
pointingPairs (x:xs) = [updatePointingPairs pairs x] ++ pointingPairs xs
                        where pairs = getPairs x x

getPairs :: Fila -> Fila -> [[Int]]
getPairs [] ys = []
getPairs (Nota x:xs) ys
              | length x == 2 && isThisPair x ys == 2 = [x]++getPairs xs ys
              | otherwise = getPairs xs ys
getPairs (Definitivo x:xs) = getPairs xs ys

isThisPair :: [Int] -> Fila -> Int
countPairs as [] = 0
countPairs as (Nota x:xs)
            | as == x = 1 + countPairs as xs
            | otherwise = countPairs as xs
countPairs as (Definitivo x:xs) = countPairs as xs

updatePointingPairs :: [[Int]] -> Fila -> Fila
updatePointingPairs [] xs = xs
updatePointingPairs (a:as) xs = updatePointingPairs as (updateOnePointingPair a xs)

updateOnePointingPair :: [Int] -> Fila -> Fila
updateOnePointingPair as (Nota x:xs) = [Nota y | y <- x, y `notElem` as]



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



{-
[[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 3,Nota [1,2,3,4,5,6,7,8,9],Definitivo 2,Nota [1,2,3,4,5,6,7,8,9],Definitivo 6,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]],[Definitivo 9,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 3,Nota [1,2,3,4,5,6,7,8,9],Definitivo 5,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 1],[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 1,Definitivo 8,Nota [1,2,3,4,5,6,7,8,9],Definitivo 6,Definitivo 4,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]],[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 8,Definitivo 1,Nota [1,2,3,4,5,6,7,8,9],Definitivo 2,Definitivo 9,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]],[Definitivo 7,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 8],[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 6,Definitivo 7,Nota [1,2,3,4,5,6,7,8,9],Definitivo 8,Definitivo 2,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]],[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 2,Definitivo 6,Nota [1,2,3,4,5,6,7,8,9],Definitivo 9,Definitivo 5,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]],[Definitivo 8,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 2,Nota [1,2,3,4,5,6,7,8,9],Definitivo 3,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 9],[Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9],Definitivo 5,Nota [1,2,3,4,5,6,7,8,9],Definitivo 1,Nota [1,2,3,4,5,6,7,8,9],Definitivo 3,Nota [1,2,3,4,5,6,7,8,9],Nota [1,2,3,4,5,6,7,8,9]]]

[[Nota [1,2,3],Definitivo 2,Nota [1,2,3]],[Definitivo 1,Nota [1,2,3],Nota [1,2,3]],[Nota [1,2,3],Nota [1,2,3],Nota [1,2,3]],[Nota [1,2,3],Definitivo 1,Definitivo 3]]
-}