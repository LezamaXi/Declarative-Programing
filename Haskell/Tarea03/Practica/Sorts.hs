module Sorts where
import Data.Maybe

--Un arreglo es una función de los enteros en un tipo 'a'; la segunda componente es para conocer su longitud.
data Arreglo a = Arr (Int->a) Int

arr = Arr f 10 where 
          f n = case n of
                 0 -> 1
                 1 -> 2
                 2 -> 23
                 3 -> 1
                 4 -> 0
                 5 -> -1
                 6 -> 10
                 7 -> 11
                 8 -> 22
                 9 -> 9
                 _ -> error "fuera de índice"

g3 =  Arr f 10 where 
          f n = case n of
                 0 -> 1
                 1 -> 5
                 2 -> 3
                 3 -> 5
                 4 -> 6
                 5 -> -10
                 6 -> 10
                 7 -> 11
                 8 -> 22
                 9 -> 9
                 _ -> error "fuera de índice"

g4 =  Arr f 10 where 
          f n = case n of
                 0 -> 1
                 1 -> 2
                 2 -> 3
                 3 -> 4
                 4 -> 5
                 5 -> 6
                 6 -> 7
                 7 -> 8
                 8 -> 9
                 9 -> 10
                 _ -> error "fuera de índice"

--Para pintar los arreglos de forma bonita
instance (Show a) => Show (Arreglo a) where
      show (Arr f n) =  "{"++ pinta 0 n f where 
                              pinta i n f | n==0 = "}" 
                                          | i==0 = show (f i)++pinta (i+1) n f 
                                          | i==n = "}"
                                          | otherwise = ","++show (f i)++pinta (i+1) n f

--Para obtener el elemento en la i-ésima posición de un arreglo
get::Arreglo a->Int->a
get (Arr f n) i | i>=0 && i < n = f i
                | otherwise = error "Array Index Out Of Bounds"   

--Para sobreescribir el elemento en la i-ésima posición
upd::Arreglo a->Int->a->Arreglo a
upd (Arr f n) i x = Arr (\m -> if m==i then x else f m) n


--Para obtener el tamaño de un arreglo
size::Arreglo a->Int
size (Arr _ n) = n

--Para saber si un elemento pertenece a un arreglo
elemArr::Eq a=>a->Arreglo a->Int
elemArr x arr = busca x 0 arr where
                busca x i arr | i < size arr = if x == get arr i then i else busca x (i+1) arr      
                              | otherwise = -1


--Para obtener la posición del mínimo en un arreglo que está en la posición 'i' en adelante.
minArr::Ord a=>Arreglo a->Int->Int
minArr arr i = buscaMin i (i+1) arr where
               buscaMin m_i j arr | j==size arr = m_i
                                  | otherwise = if get arr m_i < get arr j then buscaMin m_i (j+1) arr else buscaMin j (j+1) arr               

--Intercambia los elementos en la posición 'i' y 'j' de un arreglo.
swap::Arreglo a->Int->Int->Arreglo a
swap arr i j = let xi = get arr i
                   xj = get arr j
                   f' n | n == i = xj
                        | n == j = xi
                        | otherwise = get arr n in
               Arr f' (size arr)

--Ordena un arreglo con selectionSort
selectionSort::Ord a=>Arreglo a->Arreglo a
selectionSort arr = ordena 0 arr where
                    ordena i arr | i<size arr = let m = minArr arr i 
                                                    arr' = swap arr i m in 
                                                ordena (i+1) arr'              
                                 | otherwise = arr

-- Ordena un Arreglo por buscqueda Binaria
busquedaBinaria :: Ord a => Arreglo a -> a -> Maybe Int
busquedaBinaria arr e = busBinAux arr e 0 ((size arr)-1)

-- Auxiliar para busqueda Binaria
busBinAux :: Ord a => Arreglo a -> a -> Int -> Int-> Maybe Int
busBinAux arr e a b
  | a > b = Nothing
  | get arr (div (a+b)2) == e = Just (div (a+b) 2) 
  | get arr (div (a+b) 2) > e = busBinAux arr e a ((div (a+b) 2)-1)
  | otherwise = busBinAux arr e ((div (a+b) 2)+1) b




------------------ Mi intento de implementación de HeapSort --------------

maximo :: (Ord a) => Arreglo a -> Int -> Int -> Int
maximo arr i n = let max = if (izq <= n) && ((get arr izq) > (get arr i)) then izq else i
                 in if(der <= n) && ((get arr der) > (get arr max)) then der else max
                 where izq = 2 * i + 1 
                       der = 2 * i + 2 


heapify :: Ord a => Arreglo a -> Int -> Int -> Arreglo a
heapify arr 0 _ = arr
heapify arr i n = if max' /= i then heapify (swap arr max' i) max' n else arr 
                  where max' = maximo arr i n


creaHeap ::  Ord a => Arreglo a -> Int -> Arreglo a
creaHeap arr 0 = heapify arr 0 (size arr)
creaHeap arr i = creaHeap  (heapify arr i (size arr)) (i-1)


auxheapSort arr i = let swapped = (swap arr i 0) 
                    in if i/=1 then auxheapSort (heapify swapped i 0) (i-1) else (heapify swapped i 0)


heapSort arr = let heap = creaHeap arr ((size arr) `div` 2) in auxheapSort heap ((size arr) - 1)  


ordenado = heapSort g3


------------------------------------------------------------------------------------


arregloVacio :: Int -> a -> Arreglo a
arregloVacio n t = (Arr (\x -> if x >= 0 && x < n
                             then t
                             else error "Array Index Out Of Bounds") n)

pasaArreglo :: [Integer] -> Arreglo Integer
pasaArreglo ls = Arr (\n -> ls!!n) (length ls)

{-

arregloVacio :: Int -> a -> Arreglo a
arregloVacio n t = (Arr (\x -> if x >= 0 && x < n
                             then t
                             else error "Array Index Out Of Bounds") n)

heapSort :: (Ord a) => Arreglo a -> Arreglo a
heapSort arr@(Arr f n) = creaHeap arr 0 (arregloVacio n (f 0)) 

creaHeap :: (Ord a) => Arreglo a -> Int -> Arreglo a -> Arreglo a
creaHeap arr i heap
  | i == (size arr) = heap
  | otherwise = let new = upd heap i (get arr i) in
                  creaHeap arr (i+1) (heapifyUp new i)

getPadre :: Arreglo a -> Int -> a
getPadre arr i = get arr (div i 2)

heapifyUp :: (Ord a) => Arreglo a -> Int -> Arreglo a
heapifyUp arr i
  | i == 0 = arr
  | otherwise = let padre = getPadre arr i
                    p_i = (div i 2)
                    elem = get arr i in
                  if elem < padre
                  then heapifyUp (swap arr p_i i) p_i
                  else arr
                  
leaf :: Arreglo a -> Int -> Bool
leaf (Arr f n) i = (2*i+1) > n

getLeft :: Arreglo a -> Int -> a
getLeft arr i = get arr ((2*i)+1)

getRight :: Arreglo a -> Int -> a
getRight arr i = get arr (2*(i+1))
                  
heapifyDown :: (Ord a) => Arreglo a -> Int -> Arreglo a
heapifyDown arr i
  | leaf arr i = arr
  | otherwise = let left = getLeft arr i
                    l_i = (2*i+1)
                    right = getRight arr i
                    r_i = (2*(i+1))
                    elem = get arr i in
                  if left < elem
                  then heapifyDown (swap arr l_i i) l_i
                  else if right <= elem
                          then heapifyDown (swap arr r_i i) r_i
                               else arr

sacaHeap :: (Ord a) => Arreglo a -> Int -> Arreglo a -> Arreglo a
sacaHeap heap i orden
  | i == (size heap)-1 = upd orden i (get heap i)
  | otherwise = let n = size heap in heap
                  
-}                 
 
-- Sea A = {a1, a2, . . . , an} un conjunto de naturales tales que 1 ≤ ai ≤ n2. 
-- Muestra un algoritmo que ordene A en tiempo O(n).                  
ejer1 :: Arreglo Int -> Arreglo Int
-- (Recuerden lo que falta)
ejer1 (Arr f n) = aplicaBase (selectionSort $ (aplicaBase arr 10 n)) n 10

aplicaBase :: Arreglo Int -> Int -> Int -> Arreglo Int
aplicaBase  (Arr f s) b c = (Arr (fmap (\x -> base b c x) f) s)

base :: Int -> Int -> Int -> Int
base a b n = ((div n b)*a)+rem n b

ejer1_e = ejer1 g3 

-- Dados A y B conjuntos de enteros, |A| = n = |B|, y un x ∈ Z, diseña un algoritmo que 
-- en tiempo O(nlog(n)) encuentre dos números a ∈ A, b ∈ B tales que a + b = x.
-- Asumimos que A y B ya estan ordenados
ejer2 :: Arreglo Int -> Arreglo Int -> Int -> (Int, Int)
ejer2 a@(Arr f n) b@(Arr f' n') x = head[(a, b) | a <- [0..n-1], b <- [0..n'-1], a + b == x]

ejer2_e = ejer2 g3 g4 5

-- Sea [a1, a2, . . . , an] un arreglo de enteros y x ∈ Z. Muestra un algoritmo que 
-- en tiempo O(nlog(n)) encuentrelapareja(ai,aj) talque s.
ejer3 :: Arreglo Int -> Arreglo Int -> Int -> Maybe (Int, Int)
ejer3 a@(Arr f n) b@(Arr f' n') x = busca 0 a b x
                                    where busca i a b x | (i < size a) = if (indice_elem == Nothing) then busca (i+1) a b x else Just (get a i , get b (fromJust indice_elem))
                                                        | otherwise = Nothing
                                                        where indice_elem = busquedaBinaria b (x - (get a i))

ejer3_e = ejer3 g3 g4 5

-- Sea A un arreglo con n elementos, cada elemento en el arreglo es verde, rojo o azul

ejer4 :: Arreglo String -> Arreglo String
ejer4 arr = let tam = cuentaArr arr 0 (0,0,0) in
              case tam of
                (x,y,z) -> Arr (\i -> if i >= 0 && i <= x-1
                                          then "Verde"
                                          else if i >= x && i <= x+y-1
                                            then "Rojo"
                                            else "Azul") (x+y+z)

cuentaArr :: Arreglo String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
cuentaArr arr i tern@(x,y,z)
  |i < size arr = let e = get arr i in
                    case e of
                      "Verde" -> cuentaArr arr (i+1) (x+1, y, z)
                      "Rojo" -> cuentaArr arr (i+1) (x, y+1, z)
                      _ -> cuentaArr arr (i+1) (x, y, z+1)
  |otherwise = tern


prueba = Arr f 10 where
          f n = case n of
                 0 -> "Verde"
                 1 -> "Azul"
                 2 -> "Rojo"
                 3 -> "Azul"
                 4 -> "Rojo"
                 5 -> "Verde"
                 6 -> "Verde"
                 7 -> "Rojo"
                 8 -> "Azul"
                 9 -> "Azul"
                 _ -> error "fuera de índice"

ejer4_e = ejer4 prueba

-- Muestra que es posible multiplicar dos polinomios lineales ax + b y cx + d 
-- sin usar ma ́s de tres multipli- caciones.

ejer5 :: [Int]  -> [Int] -> [Int]
ejer5 (a:b:(xs)) (c:d:(xs')) = let a' = a*c
                                   b' = b*d
                                   c' = (a+b)*(c+d) in
                                   [a',c'-a'-b',b']
