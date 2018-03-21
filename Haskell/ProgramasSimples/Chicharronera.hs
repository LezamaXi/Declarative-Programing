module Chicharronera where

-- | C. Tipo de datos que representa los numeros complejos. 
type C = (Double, Double)
-- | P2. Tipo que representa los polinomios de segundo grado -- en el campo de los numeros reales, es decir,
-- (a,b,c) representa ax^2+bx+c.
type P2 = (Double,Double,Double)
-- | R. Tipo que representa las dos posibles raices de un -- polinomio de segundo grado.
type R = (C,C)

-- | complex1. Función que sirve para sacar la raíz positiva cuando esta se encuentre en el campo 
-- | de los números complejos. 
complex1:: P2 -> C
complex1 (a,b,c) = ((-b)/2*a, sqrt((b^2 - 4*a*c)*(-1))/2*a)

-- | complex1. Función que sirve para sacar la raíz negativa cuando esta se encuentre en el campo 
-- | de los números complejos. 
complex2:: P2 -> C
complex2 (a,b,c) = ((-b)/2*a, (-1)*sqrt((b^2 - 4*a*c)*(-1))/2*a)

-- | roots. Función que te da las dos raíces de un polinomio de segundo grado, sea o no sus raices complejas,
-- | con el formato usado una tupla de tuplas raices::=((parte real, parte imaginaria),(parte real, parte imaginaria)).
roots :: P2-> R
roots (a,b,c) = if (b^2 - 4*a*c >= 0) 
				then ((((-b)+sqrt(b^2 - 4*a*c))/2*a, 0), ((((-b)-sqrt(b^2 - 4*a*c))/2*a), 0))
				else (complex1(a,b,c), complex2(a,b,c))

-- | esreal. Función con salida Booleana que verifica si un polinomio tiene raices reales.
esreal:: P2 -> Bool
esreal (a,b,c) = if (b^2 - 4*a*c >= 0)
					then True
					else False
