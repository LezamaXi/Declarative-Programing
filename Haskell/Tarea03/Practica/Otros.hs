module Otros where
import Data.List
-- Si logras pintar en pantalla la pirámide bonita, tienes un punto extra.
dibuja :: String -> String
dibuja s = spaces ++ s where spaces = replicate ((60 - length s) `div` 2) ' '


auxpascal :: [[Int]]
auxpascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1]

pascal :: Int -> IO ()
pascal n = mapM_ putStrLn $ fmap dibuja $ fmap show $ take n auxpascal
       

-- devuelve cuántos acarreos se realizan al sumar dos enteros positivos.
acarreos :: Int -> Int -> Int
acarreos n m
  | n <= 9 = if n + m >= 10 then 1 else 0
  | m <= 9 = acarreos n m
  | otherwise = let d_n = mod n 10
                    d_m = mod m 10
                    n' = (div n 10)
                    m' = (div m 10) in
                  if d_n + d_m >= 10
                  then 1 + acarreos (div n 10) (div m 10)
                  else acarreos (div n 10) (div m 10)


-- que recibe una lista de monedas disponibles, una moneda y devuelve las 
-- distintas formas de regresar en cambio dicha moneda
cambio2 :: [Int] -> Int -> [[Int]]
cambio2 [] _ = []
cambio2 (x:xs) n = nub $ (cambio [[]] (x:xs) n) ++ (cambio2 xs n)


cambio :: [[Int]] -> [Int] -> Int -> [[Int]]
cambio acc [] _ = []
cambio acc _ 0 = acc
cambio acc (x:xs) n = if x <= n then (cambio [head acc++[x]] (x:xs) (n-x)) ++
                                     (cambio [head acc++[x]] xs (n-x))
                  else cambio acc xs n

-- Devuelve una lista con ternas (q1,q2,q3) tales que q1 + q2 + q3 = n y cada qj es un nu ́mero primo
goldbach :: Int -> Maybe [(Int, Int, Int)]
goldbach n
  | n <= 5 = Nothing
  | otherwise = Just[(a,b,c) | a <- [1..n], b <- [a..n], c <- [b..n],
                      esPrimo a, esPrimo b, esPrimo c, a+b+c == n]

esPrimo :: Int -> Bool
esPrimo n | n < 2 = False
          | n == 2 = True
          | even n = False
          | otherwise = null [ m | m <- imparesHasta n, mod n m == 0]  
          where imparesHasta n = [m | m<-[3,5..(div n 2)]]

-- Calcula la órbita de un entero, puesto que la o ́rbita de un nu ́mero es un conjunto infinito
collatz :: Int -> [Int]
collatz n = n:(calculaCollatz n)

calculaCollatz :: Int -> [Int]
calculaCollatz 1 = []
calculaCollatz n = if even n
                   then (div n 2):(calculaCollatz (div n 2))
                   else (3*n+1):(calculaCollatz (3*n+1))

-- Calcula todas las permutaciones de una lista.
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones [x] = [[x]]
permutaciones [a,b] = [[a,b],[b,a]]
permutaciones ls@(x:xs) = let perm = permutaciones xs
                          in [(inserta y i x) | y <- perm, i <- [0..(length xs)]]

inserta :: [a] -> Int -> a -> [a]
inserta ls i elem = let left = take i ls
                        right = drop i ls
                        in
                      left++(elem:right)
