procesa :: IO ()
procesa = do putStr "Nombre del sudoku: "
             nombre <- getLine
             contenido <- readFile nombre
             putStr contenido
						 --let numeros :: [Int]
               --  numeros = map read (words contenido)
             --print (qsort numeros)
