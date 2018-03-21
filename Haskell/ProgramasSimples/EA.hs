-- | Sust. Tipo que representa una sustitucion de variables en expresiones. 
type Sust = (String,EA)
-- | State. Tipo que representa el estado de una variable en la memoria de la maquina. 
-- | Se observa que este tipo es un tipo funcion.
type State = String -> Int

type Var = String

data EA = Var | Const Int | Sum EA EA | Mul EA  EA
           deriving Show


eval :: Exp -> Int -> Int
eval Var n = n
eval (Const a) n = a
eval (Sum e1 e2) n = (eval e1 n) + (eval e2 n)
eval (Mul e1 e2) n = (eval e1 n) * (eval e2 n)

sust :: EA → Sust → EA
sust Var (x,c) = if (Var == x) then c 
	else error "No es posible hacer sustitución."
sust (Sum x y) s = (sust x) + (sust y)
sust (Mul x y) s = (sust x) * (sust y)

