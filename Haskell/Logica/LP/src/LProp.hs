{-
- Implementación de la sintaxis de lógica proposicional.
- Profesora: Dra. Lourdes del Carmen González Huesca
- Ayudante: Roberto Monroy Argumedo
- Laboratorio: Fernando Abigail Galicia Mendoza
-}

module LProp where

-- | VarP. Tipo que representa el conjunto de variables proposicionales.
type VarP = Int

-- | Sust. Tipo que representa una sustitución de la lógica proposicional.
type Sust = [(VarP,Prop)]

-- | Prop. Tipo que representa el conjunto de fórmulas de la lógica
-- proposicional.
data Prop = TTrue
          | FFalse
          | V VarP
          | Neg Prop
          | Conj Prop Prop
          | Disy Prop Prop
          | Imp Prop Prop
          | Equiv Prop Prop 

instance Show Prop where 
  show (TTrue) = "T"
  show (FFalse)="F" 
  show (V x)= show x
  show (Neg p) = "¬" ++ "(" ++ show p ++ ")"
  show (Conj p1 p2) = "(" ++ show p1 ++ "^" ++show p2 ++ ")"
  show (Disy p1 p2) = "(" ++ show p1 ++ "v" ++show p2 ++ ")"
  show (Imp p1 p2) = "(" ++ show p1 ++ "=" ++show p2 ++ ")"
  show (Equiv p1 p2) = "(" ++ show p1 ++ "<->" ++show p2 ++ ")"

instance  Ord Prop where
  p1 <= p2 = peso p1 <= peso p2
  p1 > p2 = peso p1 > peso p2

instance Eq Prop where
  (==)  TTrue TTrue = True
  (==)  FFalse FFalse = True
  (==)  (V p) (V q) = (p == q)
  (==)  (Neg p) (Neg q) = (==) p q
  (==)  (Conj p1 p2) (Conj q1 q2) = ((==) p1 q1) && ((==)p2 q2)
  (==)  (Disy p1 p2) (Disy q1 q2) = ((==) p1 q1) && ((==)p2 q2)
  (==)  (Imp p1 p2) (Imp q1 q2) = ((==) p1 q1) && ((==)p2 q2)
  (==)  (Equiv p1 p2) (Equiv q1 q2) = ((==) p1 q1) && ((==)p2 q2)
  (==)  (Equiv p1 p2) (Equiv q1 q2) = ((==) p1 q1) && ((==)p2 q2)
  (==)  p1 p2 = False

-- | peso. Función que dada una fórmula devuelve el número de sus conectivos.
--
-- --> peso (Conj (V 1) (Disy (V 2) (FFalse))) = 2
-- --> peso (Conj (V 1) (Disy (V 2) (Neg (V 3)))) = 3
peso :: Prop -> Int
peso phi = case phi of
  TTrue -> 0
  FFalse -> 0
  (V _) -> 0
  (Neg p) -> 1 + peso p
  (Conj p1 p2) -> 1 + peso p1 + peso p2
  (Disy p1 p2) -> 1 + peso p1 + peso p2
  (Imp p1 p2) -> 1 + peso p1 + peso p2
  (Equiv p1 p2) -> 1 + peso p1 + peso p2

-- | nIE. Función que dada una fórmula devuelve el número de implicaciones y equivalencias.
--
-- --> nIE (Imp (V 1) (Disy (V 2) (FFalse))) = 1
-- --> nIE (Equiv (V 1) (Equiv (V 2) (FFalse))) = 2
nIE :: Prop -> Int
nIE phi = case phi of
  TTrue -> 0
  FFalse -> 0
  (V _) -> 0
  (Neg p) -> 1 + nIE p
  (Conj p1 p2) -> nIE p1 + nIE p2
  (Disy p1 p2) -> nIE p1 + nIE p2
  (Imp p1 p2) -> 1 + nIE p1 + nIE p2
  (Equiv p1 p2) -> 1 + nIE p1 + nIE p2

-- | elimEquiv. Funció que dada una fórmula devuelve su equivalente que no contiene equivalencias.
--
-- --> elimEquiv (Equiv (V 1) (Equiv (V 2) (FFalse))) =
-- --> Conj (Imp (V 1) (Conj (Imp (V 2) FFalse) (Imp FFalse (V 2)))) (Imp (Conj (Imp (V 2) FFalse) (Imp FFalse (V 2))) (V 1))
-- --> elimEquiv (Imp (V 1) (Disy (V 2) (FFalse))) =
-- --> Imp (V 1) (Disy (V 2) FFalse)
elimEquiv :: Prop -> Prop
elimEquiv phi = case phi of
  TTrue -> TTrue
  FFalse -> FFalse
  (V x) -> V x
  (Neg p) -> Neg (elimEquiv p)
  (Conj p1 p2) -> Conj (elimEquiv p1) (elimEquiv p2)
  (Disy p1 p2) -> Disy (elimEquiv p1) (elimEquiv p2)
  (Imp p1 p2) -> Imp (elimEquiv p1) (elimEquiv p2)
  (Equiv p1 p2) -> let (p1',p2') = (elimEquiv p1,elimEquiv p2) in Conj (Imp p1' p2') (Imp p2' p1')

-- | elimImp. Funció que dada una fórmula devuelve su equivalente que no contiene implicaciones.
--
-- --> elimImp (Imp (V 1) (Imp (V 2) (FFalse))) =
-- --> Disy (Neg (V 1)) (Disy (Neg (V 2)) FFalse)
-- --> elimImp (Imp (V 1) (Disy (V 2) (FFalse))) =
-- --> Disy (Neg (V 1)) (Disy (V 2) FFalse)
elimImp :: Prop -> Prop
elimImp phi = case phi of
  TTrue -> TTrue
  FFalse -> FFalse
  (V x) -> V x
  (Neg p) -> Neg (elimImp p)
  (Conj p1 p2) -> Conj (elimImp p1) (elimImp p2)
  (Disy p1 p2) -> Disy (elimImp p1) (elimImp p2)
  (Imp p1 p2) -> let (p1',p2') = (elimImp p1,elimImp p2) in Disy (Neg p1') p2'
  (Equiv p1 p2) -> Equiv (elimImp p1) (elimImp p2)

-- | elimIE. Función que dada una fórmula devuelve su equivalente que no contiene implicaciones,
-- ni equivalencias.
--
-- --> elimIE (Imp (V 1) (Equiv (V 2) (FFalse))) =
-- --> Disy (Neg (V 1)) (Conj (Disy (Neg (V 2)) FFalse) (Disy (Neg FFalse) (V 2)))
-- --> elimIE (Syss (V 1) (Equiv (V 2) (FFalse))) =
-- --> Conj (Disy (Neg (V 1)) (Conj (Disy (Neg (V 2)) FFalse) (Disy (Neg FFalse) (V 2))))
-- --> (Disy (Neg (Conj (Disy (Neg (V 2)) FFalse) (Disy (Neg FFalse) (V 2)))) (V 1))
elimIE :: Prop -> Prop
elimIE  = elimImp.elimEquiv




