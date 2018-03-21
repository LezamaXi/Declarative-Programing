--Logica Computacional 2017-2
--Tema : Implementacion de la sintaxis de la logica
--de predicados (logica de primer orden [LPO])
--Profesor: Lourdes del Carmen Gonzaléz Huesca
--Ayudante: Roberto Monroy Argumedo
--Laboratorio: Fernando A. Galicia Mendoza
module LPO where

--Modulo de listas brindados por el lenguaje
import Data.List

--Tipo que representa el conjunto de índices de VarP.
type Ind = Int

--Tipo que representa el nombre.
type Nombre = String

--Tipo que representa un término.
data Term = V Ind | F Nombre [Term] deriving(Show,Eq)

--Tipo que representa una fórmula de LPO.
data Form = TrueF
          | FalseF
          | Pr Nombre [Term]
          | Eq Term Term
          | Neg Form
          | Conj Form Form
          | Disy Form Form
          | Imp Form Form
          | Equi Form Form
          | All Ind Form
          | Ex Ind Form deriving(Show,Eq)

-- | consT. Función que devuelve una lista con todos los nombres de constantes
-- que figuran en t.
consT :: Term -> [Nombre]
consT (V _) = []
consT (F c []) = [c]
consT (F _ ts) = consT_aux ts where
  consT_aux [] = []
  consT_aux (t:ts) = union (consT t) (concat (map consT ts))

-- | funT. Función que devuelve una lista con todos los nombres de funtantes
-- que figuran en t.
funT :: Term -> [Nombre]
funT (V _) = []
funT (F c []) = []
funT (F f ts) = union [f] (funT_aux ts) where
  funT_aux [] = []
  funT_aux (t:ts) = union (funT t) (concat (map funT ts))

-- | varT. Función que devuelve una lista con todos los índices de variables que
-- figuran en t.
varT :: Term -> [Ind]
varT (V x) = [x]
varT (F f ts) = varT_aux ts where
  varT_aux [] = []
  varT_aux (t:ts) = union (varT t) (varT_aux ts)

-- | consT. Función que devuelve una lista con todos los nombres de constantes
-- que figuran en f.
consF :: Form -> [Nombre]
consF TrueF = []
consF FalseF = []
consF (Pr p t) = concat $ map consT t --Alternativa: concat [consT t | t <- ts]
consF (Eq t1 t2) = union (consT t1) (consT t2)
consF (Neg f) = consF f
consF (Conj f1 f2) = union (consF f1) (consF f2)
consF (Disy f1 f2) = union (consF f1) (consF f2)
consF (Imp f1 f2) = union (consF f1) (consF f2)
consF (Equi f1 f2) = union (consF f1) (consF f2)
consF (All x f) = consF f
consF (Ex x f) = consF f

-- | funT. Función que devuelve una lista con todos los nombres de funtantes
-- que figuran en t.
funF :: Form -> [Nombre]
funF TrueF = []
funF FalseF = []
funF (Pr p t) = concat $ map funT t --Alternativa: concat [funT t | t <- ts]
funF (Eq t1 t2) = union (funT t1) (funT t2)
funF (Neg f) = funF f
funF (Conj f1 f2) = union (funF f1) (funF f2)
funF (Disy f1 f2) = union (funF f1) (funF f2)
funF (Imp f1 f2) = union (funF f1) (funF f2)
funF (Equi f1 f2) = union (funF f1) (funF f2)
funF (All x f) = funF f
funF (Ex x f) = funF f

-- | varT. Función que devuelve una lista con todos los índices de variables que
-- figuran en t.
varF :: Form -> [Ind]
varF TrueF = []
varF FalseF = []
varF (Pr p t) = concat $ map varT t --Alternativa: concat [varT t | t <- ts]
varF (Eq t1 t2) = union (varT t1) (varT t2)
varF (Neg f) = varF f
varF (Conj f1 f2) = union (varF f1) (varF f2)
varF (Disy f1 f2) = union (varF f1) (varF f2)
varF (Imp f1 f2) = union (varF f1) (varF f2)
varF (Equi f1 f2) = union (varF f1) (varF f2)
varF (All x f) = varF f
varF (Ex x f) = varF f

-- | vf. Función que devuelve una lista con todas las varibles libres.
vf :: Form -> [Ind]
vf TrueF = []
vf FalseF = []
vf (Pr p t) = concat $ map varT t
vf (Eq t1 t2) = union (varT t1) (varT t2)
vf (Neg f) = vf f
vf (Conj f1 f2) = union (vf f1) (vf f2)
vf (Disy f1 f2) = union (vf f1) (vf f2)
vf (Imp f1 f2) = union (vf f1) (vf f2)
vf (Equi f1 f2) = union (vf f1) (vf f2)
vf (All x f) = delete x (varF f)
vf (Ex x f) = delete x (varF f)

-- | bv. Función que devuelve una lista con todas las variables ligadas.
bv :: Form -> [Ind]
bv TrueF = []
bv FalseF = []
bv (Pr p t) = []
bv (Eq t1 t2) = union (varT t1) (varT t2)
bv (Neg f) = bv f
bv (Conj f1 f2) = union (bv f1) (bv f2)
bv (Disy f1 f2) = union (bv f1) (bv f2)
bv (Imp f1 f2) = union (bv f1) (bv f2)
bv (Equi f1 f2) = union (bv f1) (bv f2)
bv (All x f) = x : (bv f)
bv (Ex x f) = x : (bv f)
