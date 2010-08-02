{-# LANGUAGE ScopedTypeVariables, ViewPatterns, NoMonomorphismRestriction #-}
module Cuts where

import Control.Arrow ((&&&),(***))
import Data.List (sortBy, minimumBy, tails, find, group, sort, init)
import Data.Ord (comparing)
import Data.Function (on)
import Control.Monad (ap,filterM)
import Test.QuickCheck (quickCheck, (==>), (.&.))

import Debug.Trace
-- tit x = trace (show x) x

-- | Mescola due liste ordinate. Le liste rimangno ordinate. Generalizzata sull'operatore di ordinamento. Se le liste non sono ordinate con lo stesso operatore, il risultato è indefinito
unionOrderedBy 	:: (t -> t -> Ordering) -- ^ operatore di ordinamento
		-> [t] 			-- ^ prima lista
		-> [t] 			-- ^ seconda lista
		-> [t]			-- ^ lista risultante

unionOrderedBy f [] xs = xs
unionOrderedBy f xs [] = xs
unionOrderedBy f (x:xs) (y:ys) = case f x y of
	LT -> x:unionOrderedBy f xs (y:ys)
	EQ -> x:y:unionOrderedBy f xs ys
	GT -> y:unionOrderedBy f (x:xs) ys



-- | Seleziona la prima condizione di coppia incontrabile in una lista. Generalizzata sull'ordinamento. Fallisce con liste vuote. Se la condizione non viene riscontrata in nessuna coppia adiacente, torna l'ultimo elemento.
condition 	:: (a -> t) 	-- ^ proiezione nello spazio ordinante
		-> (t -> t -> Bool) -- ^ condizione di successo
		-> [a] 		-- ^ lista 
		-> a		-- ^ primo elemento della condizione di coppia trovato

condition p c (map (p &&& id) -> xs) = case (snd . fst) `fmap` find (uncurry (c `on` fst)) (zip `ap` tail $ xs) of
	Nothing -> snd . last $ xs -- hack, si assume l'ultimo valore
	Just x ->  x


-- | seleziona l'insieme che somma più precisamente ad un monoide dato. Generalizzato da un operatore
bestsubset :: (Ord t, Num t, Show a) 
	=> (a -> t) 	-- ^ proiezione nello spazio delle misure
	-> t 		-- ^ obiettivo nello spazio delle misure
	-> [a] 		-- ^ elementi
	-> [a]
bestsubset _ d [] = []
bestsubset f d xs = snd . head . cuts . sortBy (comparing fst) . map (f &&& id) $ xs where
	delta (v,x) = abs (d - v)
	cuts [(v,x)] = [(v,[x])]
	cuts ((v,x):(cuts -> cxs)) =  condition (delta . head) (<) ts where
			ncs = map ((+ v) *** (x:)) cxs ++ [(v,[x])] -- vale metterlo in coda solo se è il piu piccolo
			ts = init . tails $ unionOrderedBy (flip $ comparing fst) ncs cxs


------------------------- test zone -----------------------------------------------------
testUnionOrderedBy xs ys = unionOrderedBy compare (sort xs) (sort ys) == sort (xs ++ ys)
	where types = xs :: [Float]

testCondition xs ys = (not $ null xs) && (not $ null ys) ==> condition id (<) (sortBy (flip compare) xs ++ sort ys) == minimum (xs ++ ys) where
	types = (xs :: [Float])

testBestsubset xs l = not (null xs) && all (>0) (l:xs) ==> 0.001 > abs (
			delta l (bestsubset id l xs) - 
			delta l (minimumBy (comparing $ delta l) (tail $ filterM (const [False,True]) xs))
			)
	where 	delta l xs = abs (l - sum xs)
		types = (l :: Float,xs :: [Float])

test :: IO ()
test = quickCheck $ testUnionOrderedBy .&. testCondition .&. testBestsubset

----------------------------------------------------------------------------------------
