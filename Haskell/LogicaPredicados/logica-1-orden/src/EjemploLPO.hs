{-
 -Logica Computacional 2017-2
 -Ejemplo de semántica de LPO: Enteros módulo 10.
 -Profesor: Lourdes del Carmen Gonzaléz Huesca
 -Ayudante: Roberto Monroy Argumedo
 -Laboratorio: Fernando A. Galicia Mendoza
-}

--Importamos los módulos necesarios de LPO.
import LPO
import LPOSust
import LPOSem

--Universo : Z10, es decir, los enteros módulo 10
m :: [Int]
m = [0..9]

--Un estado de las variables 'x' e 'y'.
est :: Estado Int
est "x" = 1
est "y" = 2
est _ = 0

--Asignamos una interpretación a las constantes {0,...,9} y las
--funciones: identidad, suma, resta, producto y división.
iF :: IntF Int
iF s [] = case s of
  "0" -> 0
  "1" -> 1
  "2" -> 2
  "3" -> 3
  "4" -> 4
  "5" -> 5
  "6" -> 6
  "7" -> 7
  "8" -> 8
  "9" -> 9
  _ -> 0
  iF "id" [n] = n
  iF "+" [n1,n2] = (mod n1 10)+(mod n2 10)
  iF "-" [n1,n2] = (mod n1 10)-(mod n2 10)
  iF "*" [n1,n2] = (mod n1 10)*(mod n2 10)
  iF "/" [n1,n2] = div (mod n1 10) (mod n2 10)

--Asignamos una interpretación a las relaciones de menor o igual e igualdad.
iM :: IntR Int
iM "<=" [n1,n2] = n1 <= n2
iM "=" [n1,n2] = n1 == n2

--Ejemplos de interpretación de términos.
--id 1 = 1
ejemplo1 = iTerm est iF (F "id" [F "1" []])

--1+2 = 3
ejemplo2 = iTerm est iF (F "+" [F "1" [],F "2" []])

--3 / 0 = 0
ejemplo3 = iTerm est iF (F "/" [F "0" [],F "3" []])

--Ejemplos de intrepretación de teoremas.
--Para todo x en Z10, 0 <= x
ejemplo4 = iForm m (est,iF,iM) (All "x" (Pr "<=" [F "0" [],V "x"]))

--Para todo x en Z10, existe un y en Z10 tal que x <= y
ejemplo5 = iForm m (est,iF,iM) (All "x" (Ex "y" (Pr "<=" [V "x", V "y"])))

--Falso: Para todo x en Z10, 0 = x 
ejemplo6 = iForm m (est,iF,iM) (All "x" (Pr "=" [F "0" [], V "x"]))

--Para todo x en Z10, 0 = x - x
ejemplo7 = iForm m (est,iF,iM) (All "x" (Pr "=" [F "0" [], F "-" [V "x", V "x"]]))

--Para todo x en Z10, x = id(x)
ejemplo8 = iForm m (est,iF,iM) (All "x" (Pr "=" [V "x", F "id" [V "x"]]))

--Para todo x en Z10, existe un y en Z10, tal que x = y + y
ejemplo9 = iForm m (est,iF,iM) (All "x" (Ex "y" (Pr "=" [V "x", F "+" [V "y",V "y"]])))
