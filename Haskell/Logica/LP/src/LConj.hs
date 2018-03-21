{-
- Implementación de conjuntos con listas.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
-}

module LConj where

-- | equivL. Función que determina si dos listas son equivalentes.
equivL :: Eq a => [a] -> [a] -> Bool
equivL l1 l2 = e_aux l1 l2 && e_aux l2 l1

-- e_aux. Función que dadas dos listas determina si todos los
-- elementos de la primera están en la segunda.
e_aux :: Eq a => [a] -> [a] -> Bool
e_aux [] l2 = True
e_aux (x:xs) l2 = x `elem` l2 && e_aux xs l2

-- | filtro. Función que dado un predicado y una lista l, devuelve
-- la lista de los elementos de l que cumplen con el predicado.
filtro :: (a -> Bool) -> [a] -> [a]
filtro p [] = []
filtro p (x:xs)
  | p x = x:filtro p xs
  | otherwise = filtro p xs

-- | nElem. Función que determina si un elemento no está en la lista.
nElem :: Eq a => a -> [a] -> Bool
nElem x l = not (elem x l)

-- | diff. Función que devuelve la diferencia de listas.
diff :: Eq a => [a] -> [a] -> [a]
diff l1 l2 = [x | x <- l1, nElem x l2]

-- | union. Función que devuelve la union de listas.
union :: Eq a => [a] -> [a] -> [a]
union [] l = l
union (x:xs) l
  | elem x l = union xs l
  | otherwise = x:union xs l

-- | inter. Función que devuelve la intersección de listas.
inter :: Eq a => [a] -> [a] -> [a]
inter l1 l2 = [x | x <- l1, x `elem` l2]

-- | subconj. Función que devuelve la potencia de una lista
subconj :: [a] -> [[a]]
subconj [] = [[]]
subconj (x:xs) = xs' ++ map (x:) xs' where
  xs' = subconj xs
