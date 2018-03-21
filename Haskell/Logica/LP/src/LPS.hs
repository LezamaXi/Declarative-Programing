{-
- Implementación de la semántica de lógica proposicional.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
-}

module LPS where

import LProp
import LConj

-- | Estado. Tipo que representa un estado de variables.
type Estado = [VarP]

-- | i. Implementación de la función de interpretación.
i :: Estado -> Prop -> Bool
i _ TTrue = True
i _ FFalse = False
i e (V x) = x `elem` e
i e (Neg p) = not (i e p)
i e (Conj p1 p2) = (i e p1) && (i e p2)
i e (Disy p1 p2) = (i e p1) || (i e p2)
i e (Imp p1 p2) = (not (i e p1)) || (i e p2)
i e (Equiv p1 p2) = (i e (Imp p1 p2)) && (i e (Imp p2 p1))

-- | vars. Función que devuelve el conjunto de variables proposicionales de una
-- fórmula.
vars :: Prop -> [VarP]
vars (V x) = [x]
vars (Neg p) = vars p
vars (Conj p1 p2) = union (vars p1) (vars p2)
vars (Disy p1 p2) = union (vars p1) (vars p2)
vars (Imp p1 p2) = union (vars p1) (vars p2)
vars (Equiv p1 p2) = union (vars p1) (vars p2)
vars _ = []

-- | estados. Función que devuelve todos los posibles estados de una fórmula.
estados :: Prop -> [Estado]
estados p = subconj $ vars p

-- | modelos. Función que devuelve todos los posibles modelos de una fórmula.
modelos :: Prop -> [Estado]
modelos p = [e | e <- estados p, i e p]

-- | tautologia. Función que indica si una fórmula es una tautología.
tautologia :: Prop -> Bool
tautologia p = equivL (estados p) (modelos p)

-- | satisfen. Función que determina si una fórmula es satisfacible en un
-- estado dado.
satisfen :: Estado -> Prop -> Bool
satisfen e p = i e p

-- | satisf. Función que determina si una fórmula es satisfacible.
satisf :: Prop -> Bool
satisf p = modelos p /= []

-- | insatisfen. Función que determina si una fórmula es insatisfacible en un
-- estado dado.
insatisfen :: Estado -> Prop -> Bool
insatisfen e p = not $ satisfen e p

-- | satisf. Función que determina si una fórmula es una contradicción.
contrad :: Prop -> Bool
contrad p = not $ satisf p

-- | equiv. Función que determina si dos fórmulas son equivalentes.
equiv :: Prop -> Prop -> Bool
equiv p1 p2 = tautologia (Equiv p1 p2)


modelosConj :: [Prop] -> [Estado]
modelosConj [] = []
modelosConj p = [e | e <- estadosConj p, satisfenConj e p]


satisfenConj :: Estado -> [Prop] -> Bool
satisfenConj _ [] = False
satisfenConj e (p: []) =( i e p) 
satisfenConj e (p:ps) =(i e p) && satisfenConj e ps

insatisfenConj :: Estado -> [Prop] -> Bool
insatisfenConj _ [] = True
insatisfenConj e (p: []) = (insatisfen e p)
insatisfenConj e (p:ps)= (insatisfen e p) && insatisfenConj e ps

satisfConj :: [Prop] -> Bool
satisfConj p = modelosConj p /= [] 

auxeC2 :: [Prop] -> [VarP]
auxeC2 [] = []
auxeC2 v = concat [vars p | p <- v]

estadosConj :: [Prop] -> [Estado]
estadosConj [] = []
estadosConj p = subconj (auxeC2 p) 

insatisfConj :: [Prop] -> Bool
insatisfConj [] = True
insatisfConj (p:ps) = not (satisf p) && insatisfConj (ps)





