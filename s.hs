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

procesa :: IO ()
procesa = do putStr "Dime el nombre del sudoku: "
             name <- getLine
             content <- readFile name
             let
                line = readSudoku content
             print line

--Devuelve las lineas de un String
readSudoku :: String -> [String]
readSudoku xs = lines xs

--Crea el tipo Sudoku
toSudoku :: [String] -> [Fila]
toSudoku xs = map toFila xs

toFila :: String -> Fila
toFila xs = map toCelda xs

toCelda :: Char -> Celda
toCelda x 
        | Data.Char.isDigit x && x > '0' && x < '9' = Definitivo (Data.Char.digitToInt x)
        | otherwise = Nota [1,2,3,4,5,6,7,8,9]


--Elementos definitivos de una Fila
inRow :: Fila -> [Int]
inRow [] = []
inRow ((Definitivo x):xs)  =  [x] ++ inRow xs
inRow ((Nota ys):xs) = inRow xs

--Actualiza el Sudoku
update :: Sudoku -> Sudoku
update [] = []
update (x:xs) = [updateNotaf lista x] ++ update xs
               where lista = inRow x

-- Actualiza Nota [Int] mirando la fila
updateNotaf :: [Int] -> Fila -> Fila
updateNotaf xs ((Definitivo e):es)
                         | es /= [] = [Definitivo e] ++ updateNotaf xs es
                         | otherwise = [Definitivo e]
updateNota xs ((Nota ys):es)
                         | es /= [] = [Nota (eliminaElems xs ys)] ++ updateNotaf xs es
                         | otherwise = [Nota (eliminaElems xs ys)]

--Elimina todos los elementos de una lista de otra lista
eliminaElems :: [Int] -> [Int] -> [Int]
eliminaElems [] ys = ys
eliminaElems (x:xs) ys
        | x `elem` ys = eliminaElems xs (Data.List.delete x ys)
        | otherwise = eliminaElems xs ys


-- Muestra los Definitvo Int del Sudoku
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
