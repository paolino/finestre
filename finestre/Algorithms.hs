module Algorithms where

import Data.List (sort, foldl')
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Arrow ((&&&))
-- import Debug.Trace (trace)



type Soluzione a = [[a]]

type Scarto a = [a] -> a

type Enumera a = Scarto a -> [a] -> Soluzione a

firstFit :: (Num a, Ord a) => Enumera  a
firstFit y = foldl' (inserto y) []  

firstFitDec :: (Num a, Ord a) => Enumera a
firstFitDec y = firstFit y . reverse . sort

inserto :: (Num a, Ord a) => Scarto a -> Soluzione a -> a -> Soluzione a
inserto y zss x  = let
	(rs,ts) = break ((>=0) . y . (x:)) zss
	in case ts of 
		[] -> rs ++ [[x]]
		t:ts' -> rs ++ (x:t):ts'

type Finestra a = (a, a) -> [a]
type Serie a = [(Int,Finestra a,(a,a))]

fromSerie :: Serie a -> [a] 
fromSerie = concat . concatMap (\(n,t,(x,y)) -> replicate n $ t (x,y))

type ConScarto a = ([a],a)

solve :: Scarto a -> Enumera a -> Serie a -> [ConScarto a]
solve g f = map (id &&& g) . f g . fromSerie 

lPrint :: Show a  => [a] -> IO ()
lPrint = mapM_ (putStrLn . show ) . zip [1..]


