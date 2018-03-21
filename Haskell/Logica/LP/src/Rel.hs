module Rel where
import LProp
import LConj

type U a = [a]
type R a = [(a,a)]

invR :: R a -> R a
invR r = [(a,b) | (b,a) <- r] 

taux :: a -> U a -> R a
taux x [] = []
taux x (y:ys) = (x,y):taux x ys

total :: Eq a => U a -> R a
total [] = []
total (x:xs) = union(taux x (x:xs))(total xs)

refl :: Eq a => R a -> Bool
refl r = and[(elem (fst i, fst i) r) && (elem (snd i, snd i) r)| i <- r]

symm :: Eq a => R a -> Bool
symm r = and[(y,x) `elem` r | (x,y) <- r]