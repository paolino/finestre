module Cuts where

import Data.List ((\\),minimumBy)
import Data.Ord (comparing)

cuts :: (Ord t, Num t) => t -> [t] -> [[t]]
cuts l [] = []
cuts l (x:xs) = let cxs = cuts l xs 
	in cxs ++ filter ((<= l) . sum) ([x]:map (x:) cxs)

bestcut l xs = let
	cs = cuts l xs
	c = minimumBy  (comparing $ abs . (subtract l) . sum) cs
	in (c, xs \\ c)

ordbybestcut l xs = let (xc,yc) in bestcut l xs 
	in xc ++ yc
