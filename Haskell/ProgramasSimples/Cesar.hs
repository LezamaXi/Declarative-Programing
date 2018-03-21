module Cesar where
import Data.Char

-- | Función que recibe una letra mayúscula del alfabeto latino (es decir, un elemento de tipo Char) 
-- | y devuelve el entero correspondiente a la posición de la letra en el alfabeto latino, 
-- | es decir, A = 0, B = 1, ..., etc.
-- | Observación: En caso de no recibir una letra mayúscula devuelve un error.
alf :: Char -> Int
alf c = if(isUpper(c)) then ord c - ord 'A'
		else error "El programa funciona solo con mayusculas"

-- | Función que recibe un entero que represente la posición de una letra de acuerdo al alfabeto latino y 
-- | devuelve la posición de acuerdo a la función hash descrita por Suetonio (A = D).
suetonio :: Int -> Int
suetonio l = (l + 3) `mod` 25

-- | Función que recibe el enetero que representa la posición de una letra que queremos cifrar y devuelve
-- | la letra ya cifrada.
trans :: Int -> Char
trans n = chr(ord 'A' + suetonio(n))

-- | Función que recibe una palabra y devuelve su cifrado César. 
-- | Observación: Si la palabra tiene minúsculas, se convierten a mayúsculas con la funcion toUpper.
cifrado :: String -> String
cifrado xs = [trans(alf(toUpper x)) | x <- xs]