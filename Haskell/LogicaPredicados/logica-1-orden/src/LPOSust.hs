{-
 -Logica Computacional 2017-2
 -Tema : Implementacion de la sustitución.
 -Profesor: Lourdes del Carmen Gonzaléz Huesca
 -Ayudante: Roberto Monroy Argumedo
 -Laboratorio: Fernando A. Galicia Mendoza
-}
module LPOSust where

import LPO
import Data.List

-- | Subst. Tipo que representa una sustitución de variables en términos.
type Subst = [(Ind,Term)]

type SubstT = [(Term, Term)]

-- | IntF. Tipo que representa una interpretacion de formulas.
type IntF a = Nombre -> [a] -> a

-- | IntR. Tipo que representa una interpretacion de relaciones.
type IntR a = Nombre -> [a] -> Bool

-- | Estado. Tipo que representa el estado de una variable del universo.
type Estado a = Ind -> a

-- | Mundo. Tipo que representa un mundo.
type Mundo a = (Estado a, IntF a, IntR a)

-- | elimRep. Función que elimina los elementos repetidos de una lista.
elimRep :: Eq a => [a] -> [a]
elimRep l = eR_aux l [] where
  eR_aux [] c = c
  eR_aux (x:xs) c
    | elem x c = eR_aux xs c
    | otherwise = eR_aux xs (c++[x])

-- | verifSus. Función que verifica una sustitución.
verifSus :: Subst -> Bool
verifSus s = elimRep [i | (i,t) <- s] == [i | (i,t) <- s]

-- | apsubTaux. Función auxiliar que aplica una sustitución de variables en términos
-- en un término.
apsubTaux :: Term -> Subst -> Term
apsubTaux (V x) sus = case sus of
  [] -> V x
  (s:ss) -> if fst s == x then snd s else apsubTaux (V x) ss
apsubTaux (F c []) _ = F c []
apsubTaux (F f ts) s = F f (apsubTL_aux ts s) where
  apsubTL_aux [] s = []
  apsubTL_aux (t:ts) s = ((apsubTaux t s):apsubTL_aux ts s)
  
-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en un término dado.
apsubT :: Term -> Subst -> Term
apsubT t s = if verifSus s then apsubTaux t s else error "Sustitución no legal."

-- | apsubFaux. Función auxiliar que aplica una sustitución de variables en una fórmula.
apsubFaux :: Form -> Subst -> Form
apsubFaux TrueF _ = TrueF
apsubFaux FalseF _ = FalseF
apsubFaux (Pr n xs) sus = Pr n [apsubT t sus | t <- xs]
--apsubFaux (Eq f1 f2) sus = Eq (apsubFaux f1 sus) (apsubFaux f2 sus)
apsubFaux (Neg f) sus = Neg $ apsubFaux f sus
apsubFaux (Conj f1 f2) sus = Conj (apsubFaux f1 sus) (apsubFaux f2 sus)
apsubFaux (Disy f1 f2) sus = Disy (apsubFaux f1 sus) (apsubFaux f2 sus)
apsubFaux (Imp f1 f2) sus = Imp (apsubFaux f1 sus) (apsubFaux f2 sus)
apsubFaux (Equi f1 f2) sus = Equi (apsubFaux f1 sus) (apsubFaux f2 sus)
apsubFaux (All x f) sus = if not $ x `elem` cjto
                          then (All x (apsubFaux f sus))
                          else (All x f)
  where cjto = union [fst s | s <- sus] (concat [varT $ snd s | s <- sus])
apsubFaux (Ex x f) sus = if not $ x `elem` cjto
                          then (Ex x (apsubFaux f sus))
                          else (Ex x f)
  where cjto = union [fst s | s <- sus] (concat [varT $ snd s | s <- sus])


-- | apsubT. Función que aplica una sustitución de variables de variables en términos
-- en una fórmula dada.
apsubF :: Form -> Subst -> Form
apsubF f s = if verifSus s then apsubFaux f s else error "Sustitución no legal."

-- | sustTermTerm. Función que dado un término y una sustitución de términos
-- en terminos, regresa la sustitución de dicho término.
sustTermTerm :: Term -> SubstT -> Term
sustTermTerm term sust = if sust_completa /= term then sust_completa
                         else sust_terms_fun term
  where sust_completa = foldl (\acc x -> if term == fst x then snd x else acc) term sust
        sust_terms_fun term = case term of
                                (V _) -> term
                                (F _ []) -> term
                                (F n terms) -> F n [sustTermTerm t sust | t <- terms]

--Ejemplos:

--Considerese los siguientes terminos.
t1 = F "f" [V 1,F "c" [],V 3]
t2 = F "g" [t1,V 2]
t3 = F "d" []
t4 = F "h" [V 6,V 1]

--Se realizan las siguientes sustituciones validas.
sust1 = apsubT t2 [(2,t3),(1,t4)]
sust2 = apsubT t4 [(1,t1)]
sust3 = apsubT t2 [(1,t2)]

--La siguiente sustitución es inválida.
sustInv1 = apsubT t2 [(2,t3),(2,t4)]
sustInv2 = apsubT t3 [(1,V 1),(2,V 3),(1,V 4)]
