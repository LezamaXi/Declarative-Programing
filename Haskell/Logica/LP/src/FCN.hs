module FNC where

import LProp
import LConj

nN :: Prop -> Int
nN TTrue = 0
nN FFalse = 0
nN (V p) = 0
nN (Neg p) = if ((peso p) > 0) then 1 + (nN p) else (nN p)
nN (Conj x y) = (nN x) + (nN y)
nN (Disy x y) =  (nN x) + (nN y)
nN (Imp x y) =  (nN x) + (nN y)
nN (Equiv x y) =  (nN x) + (nN y)


fnn :: Prop -> Prop
fnn TTrue = TTrue
fnn FFalse = FFalse
fnn (V p) = V p
fnn (Neg p) = aux_fnn p
fnn (Conj p1 p2) = (Conj (fnn p1) (fnn p2))
fnn (Disy p1 p2) = (Disy (fnn p1) (fnn p2))
fnn p = fnn(elimIE(p))


aux_fnn :: Prop -> Prop
aux_fnn TTrue = FFalse
aux_fnn FFalse = TTrue
aux_fnn (V p) = (Neg (V p))
aux_fnn (Neg p) = (fnn p)
aux_fnn (Disy p1 p2) = (Disy (aux_fnn p1) (aux_fnn p2))


fnc :: Prop -> Prop
fnc TTrue = TTrue
fnc FFalse = FFalse
fnc (V p) = V p
fnc (Neg x) = fnc(fnn(Neg x))
fnc (Conj x y) = Conj(fnc x)(fnc y)
fnc (Disy (Conj x y) p) = fnc (Conj (Disy (fnc x) (fnc p)) (Disy (fnc y) (fnc p)))
fnc (Disy p (Conj x y)) = fnc (Conj (Disy (fnc p) (fnc x)) (Disy (fnc p) (fnc y)))
fnc p = fnc(fnn p)


