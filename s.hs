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
             print line

--Devuelve las lineas de un String
readSudoku :: String -> [String]
readSudoku xs = lines xs


--Crea el tipo Sudoku
toSudoku :: [String] -> [Fila]
toSudoku xs = zipWith toFila xs xs

toFila :: String -> String -> Fila
toFila [] _ = []
toFila (x:xs) ys
         | Data.Char.isDigit x && x >= '1' && x <= Data.Char.intToDigit (length ys) = [Definitivo (Data.Char.digitToInt x)] ++ toFila xs ys
         | otherwise = [Nota [1..n]] ++ toFila xs ys
              where n = length ys




-----------------------  UPDATE   ---------------------------
-- Lo actualiza TODO
update :: Sudoku -> Sudoku
update xs = definitivo (updateNotaf (updateNotac (juntarGrid (updateNotaGs 0 xs))))

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


-- Convierte en definitivo el único elemento de la lista Nota [Int]
definitivo :: Sudoku -> Sudoku
definitivo xs = map toDefinitivo xs

toDefinitivo:: Fila -> Fila
toDefinitivo [] = []
toDefinitivo ((Definitivo x):xs) = [Definitivo x] ++ toDefinitivo xs
toDefinitivo ((Nota x):xs)
              | length x == 1 = [Definitivo (x !! 0)] ++ toDefinitivo xs
              | otherwise = [Nota x] ++ toDefinitivo xs



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
