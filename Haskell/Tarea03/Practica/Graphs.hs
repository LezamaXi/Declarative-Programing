module Graphs where

--Tipo para construir gráficas polimórficas.
data Graph a = Graph [(a,[a])] deriving (Show, Eq)

--Dos gráficas de ejemplo.
g1 = Graph [(1,[2,3]),
            (2,[1,4]),
            (3,[1,4]),
            (4,[2,3,5]),
            (5,[4,6]),
            (6,[5])]
            
g2 = Graph [(1,[2,3]),
            (2,[1,3,4]),
            (3,[1,2,4]),
            (4,[3,5,2]),
            (5,[4])]

g3 = Graph [(1,[2,3,4]),
            (2,[3]),
            (3,[6]),
            (4,[3]),
            (5,[4]),
            (6,[2,5])]
            

--El tipo 'Graph' es un funtor
instance Functor Graph where
   fmap f = \(Graph vs) -> Graph [(f w, map f ws) | (w,ws)<-vs]        
   

--Ejemplo para hacer un fmap sobre las gráficas de arriba.
f 1 = "Lalo"
f 2 = "Lelo"
f 3 = "Lilo"
f 4 = "Lolo"
f 5 = "Lulu"
f 6 = "Lala"  
f _ = "Lero Lero"

vertex :: Graph a -> [a]
vertex (Graph g) = case g of
                     [] -> []
                     ((v,es):vs) -> v:(vertex (Graph vs))

edges :: Graph a -> [[a]]
edges (Graph g) = case g of
                     [] -> []
                     ((v,es):vs) -> es:(edges (Graph vs))
-- Checa si una grafica en conexa 
isConnected :: (Eq a) => Graph a -> Bool
isConnected (Graph [(v,es)]) = True
isConnected g@(Graph ((v,es):xs)) = let eds = concat $ edges $ (Graph xs) in
                                      elem v eds && isConnected (Graph xs)
-- Checa si los vertices son de corte
vertices_corte :: (Eq a) => Graph a -> [a]
vertices_corte g@(Graph l@((v,es):xs)) = vdc g 0 ((length l)-1)

-- Auxiliar para funcion de vertices de corte
vdc :: (Eq a) => Graph a -> Int -> Int -> [a]
vdc g@(Graph l@((v,es):xs)) i n = let el = l!!i
                                      v = fst el
                                      g' = Graph (remove l v)
                                  in
                                    if i < n
                                    then if isConnected g'
                                         then vdc g (i+1) n
                                         else v:(vdc g (i+1) n)
                                    else if isConnected g'
                                         then []
                                         else [v]

-- Quita los vertices para luego verificar si es conexa o no
remove :: (Eq a) => [(a,[a])] -> a -> [(a,[a])]
remove [(v,es)] e = if v == e then [] else [(v, [ed | ed <- es, ed /= e])]
remove ((v,es):xs) e = if v == e then remove xs e else (v, [ed | ed <- es, ed /= e]):(remove xs e)

-- devuelve todas las aristas de corte en una gráfica simple
aristas_corte::Graph->[(V,V)]
aristas_corte = error "no implementada"

-- Dada una gráfica simple, regresa el subconjunto independiente más grande.
independienteMax::Graph->[V]
independienteMax = error "no implementada"

vertices :: Graph -> [V]
vertices (Graph l) = map fst l

aristas :: Graph -> [(V,V)]
aristas (Graph l) = nub $ concat $ map adyacentes l where
  adyacentes (v, vs) = [(v, w) | w <- vs]


permutaciones :: [a] -> [[a]]
permutaciones l = permutations l

--Dada una arista y una grafica g
--  Regresa si la arista se encuentra en la g
aristaEn :: (V, V) -> Graph -> Bool
aristaEn arista g = elem arista (aristas g)

--Dada una lista de permutaciones de vertices
--  Regresa los ciclos (sucesiones de aristas) posibles con dichas permutaciones
ciclos :: [[V]] -> [[(V,V)]]
ciclos [] = []
ciclos (x:xs) = ((zip x (tail x))++[(last x, head x)]):(ciclos xs)

--Dado un ciclo (sucesion de aristas) y una grafica g
--  Regresa si dicho ciclo se encuentra en g
cicloEnGrafica :: [(V,V)] -> Graph -> Bool
cicloEnGrafica [] _ = True
cicloEnGrafica (x:xs) g = (aristaEn x g) && (cicloEnGrafica xs g)

--Dada una lista de ciclos y una grafica g
--  Regresa si ALGUN ciclo se encuentra en g
ciclosEnGrafica :: [[(V,V)]] -> Graph -> Bool
ciclosEnGrafica [] _ = False
ciclosEnGrafica (c:xs) g = (cicloEnGrafica c g) || (ciclosEnGrafica xs g)

--Sea g grafica, verifica si de todos los posibles ciclos hamiltonianos
--alguno se encuentra en g
hamilton :: Graph -> Bool
hamilton g = ciclosEnGrafica (ciclos $ permutaciones $ vertices g) g

