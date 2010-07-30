module Cuts where

import Control.Arrow
import Data.List ((\\),sort, minimumBy, partition)
import Data.Ord (comparing)
import Debug.Trace
import Control.Monad
import Test.QuickCheck

cuts :: (Ord t, Num t, Ord a) => (a -> t) -> t -> [a] -> [a]
cuts f l xs = let 
	cuts' [] = ([],[])
	cuts' (x:xs) = let 
			t = f x 
			(bos,cxs) = cuts' xs 
			(cxs',bos') = partition ((<= l) . fst) $ (t,[x]): map ((+ t) *** (x:)) cxs
			bo' = case bos' ++ bos of 
				[] -> []
				boss -> return $ minimumBy (comparing fst) boss
			in (bo',cxs ++ cxs')
	(jo,ju) = cuts' $ sort xs
	
	in snd . minimumBy  (comparing $ abs . (subtract l) . fst) $ jo ++ ju


test :: [Float] -> Float -> Property
test xs l = not (null xs) && all (>0) (l:xs) ==> 
		delta l (cuts id l xs) == 
			delta l (minimumBy (comparing $ delta l) (tail $ filterM (const [False,True]) xs))
	where delta l xs = abs (sum xs - l)


ordbybestcut f l xs = let c = cuts f l xs 
	in c ++ (xs \\ c)
