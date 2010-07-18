module Finestre where

import Data.List (sort, foldl')
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Arrow ((&&&))
-- import Debug.Trace (trace)


type Misura = Int -- millimetri

type Soluzione = [[Misura]]

type Scarto = [Misura] -> Misura

type Enumera = Scarto -> [Misura] -> Soluzione

firstFit :: Enumera 
firstFit y = foldl' (inserto y) []  

firstFitDec :: Enumera
firstFitDec y = firstFit y . reverse . sort

inserto :: Scarto -> Soluzione -> Misura -> Soluzione
inserto y zss x  = let
	(rs,ts) = break ((>=0) . y . (x:)) zss
	in case ts of 
		[] -> rs ++ [[x]]
		t:ts' -> rs ++ (x:t):ts'

type Finestra = (Misura, Misura) -> [Misura]
type Serie = [(Int,Finestra,(Misura,Misura))]

fromSerie :: Serie -> [Misura] 
fromSerie = concat . concatMap (\(n,t,(x,y)) -> replicate n $ t (x,y))

type ConScarto = ([Misura],Misura)

solve :: Scarto -> Enumera -> Serie -> [ConScarto]
solve g f = map (id &&& g) . f g . fromSerie 

lPrint :: Show a  => [a] -> IO ()
lPrint = mapM_ (putStrLn . show ) . zip [1..]


