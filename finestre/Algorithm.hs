module Algorithms (firstFit) where

import Data.List (foldl')

firstFit :: (Num a, Ord a) 	=> ([a] -> Bool) -- ^ accettazione
				-> [a] 		-- ^ elementi da piazzare
				-> [[a]]	-- ^ elementi piazzati
firstFit y = foldl' (inserto y) []  where
	inserto y zss x  = let
		(rs,ts) = break (y . (x:)) zss
		in case ts of 
			[] -> rs ++ [[x]]
			t:ts' -> rs ++ (x:t):ts'



