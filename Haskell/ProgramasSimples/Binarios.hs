module Binarios where
-- | Se define un tipo de datos que represente a BinarioPos y se agrega la expresión deriving Show.
data BinarioPos = U | Cero BinarioPos | Uno BinarioPos deriving (Show)

-- | Función recursiva que dado un número binario, devuelve su sucesor.
sucesor :: BinarioPos -> BinarioPos
sucesor U = Cero U
sucesor (Cero x) = Uno x
sucesor (Uno x) = Cero (sucesor x)

-- | Función recursiva que dados dos números binarios, devuelve su suma.
suma :: BinarioPos -> BinarioPos -> BinarioPos
suma U U = Cero U
suma (Cero x) U = Uno x 
suma (Uno x) U = Cero (sucesor x)
suma U (Cero x) = Uno x 
suma U (Uno x)= Cero (sucesor x)
suma (Cero x) (Uno y) = Uno (suma x y)
suma (Uno x) (Cero y) = Uno (suma x y)
suma (Cero x) (Cero y) = Cero (suma x y)
suma (Uno x) (Uno y) = Cero (suma (sucesor x)  (y))

-- | Función recursiva que dados dos nímeros binarios, devuelve su producto.
multip :: BinarioPos -> BinarioPos -> BinarioPos
multip U U = U
multip (Cero x) U = (Cero x)
multip (Uno x) U = (Uno x)
multip U (Cero x)= (Cero x)
multip U (Uno x)= (Uno x)
multip (Cero x) (Cero y) = Cero ((multip (Cero x)  (y)))
multip (Uno x) (Cero y) = Cero (multip (Uno x)  (y))
multip (Cero x) (Uno y) = Cero (suma (multip x U) (multip (Cero x)  (y))) 
multip (Uno x) (Uno y) = Uno (suma (x) (multip (Uno x) (y)))



