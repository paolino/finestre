module Infissi where

import Data.List ((\\),mapAccumL, sort, minimumBy, sortBy, intercalate,find,delete)
import Control.Monad
import System.Random
import Data.Ord (comparing) 
import Control.Arrow
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Text.Printf

import MBin
import Score
import Cuts


type Misura = Int -- millimetri

data Marked 
	= BiMarked {misura :: Misura} 
	| Marked {misura :: Misura} 
	| UnMarked  {misura :: Misura} deriving (Show,Eq)

markmap f (BiMarked x) = BiMarked (f x)
markmap f (Marked x) = Marked (f x)
markmap f (UnMarked x) = UnMarked (f x)

-----------------------------------------------------------------------------------
taglio :: (Misura -> Misura) -> Misura -> [Marked] -> [(Marked,Marked)]
taglio cutWaste maxWood = snd . mapAccumL k 0 . reorder . sortBy (flip $ comparing misura)
	where 	k t x = (cutWaste $ t + misura x , ((t +) `markmap` x,x))
		reorder xs = ys ++ (xs \\ ys) where
			ys = bestsubset (cutWaste . misura) (maxWood `div` 2) xs

	
showMisura x = printf "%3d,%1d" a b
	where (a,b) =  x `divMod` 10 

showTagli :: [[(Marked,Marked)]] -> String
showTagli  = unlines . map (\(n,ys) -> printf "%2d)  "  n ++  
	intercalate " " (map (\(x,_) -> showMark x) ys ) ++ replicate (max 0 ((7 - length ys) * 7)) ' ' ++ " (" ++ 
	intercalate " " (map (\(_,t) -> showMark t) ys )++ ")") . zip [(1 :: Int)..]

trace cw mw tutto xs = do
	print . take 6 . map (length &&& score) $ xs
	writeFile "soluzione" . showTagli . map (taglio cw mw) . marks tutto . head  $ xs
-------------------------------------------------
mark ::  [Marked] -> MBin Misura -> ([Marked],[Marked])
mark ys = mapAccumL f ys . sviluppo . (\(MBin _ _ b) -> b) where
	f ys x = case find ((== x) . misura) ys of
		Nothing -> error "incoerenza 4"
		Just y -> (delete y ys,y)

marks :: [Marked] -> [MBin Misura] -> [[Marked]]
marks ys = snd . first alert . mapAccumL mark ys where
	alert [] = ()
	alert _ = error "incoerenza 5"

showMark (UnMarked x) = showMisura x ++ " "
showMark (Marked x) = showMisura x ++ "s"
showMark (BiMarked x) = showMisura x ++ "d"

------------------------------------------------------------

kx = 46
kys = 88
kyd = 74

singoleI n t1 t2 (x,y) = replicate t1 (x - kx * n) ++ replicate t2 (y - kys) 
doppieI n t1 t2 (x,y) = replicate t1 (x - kx * n) ++ replicate t2 ((y - kyd) `div` 2)
terzolato x y = floor . sqrt $ (fromIntegral y ^ 2 + fromIntegral x ^ 2)
---------------------------------------------------------------------
--- finestre ............
---------------------------------------------

telaioFinestra (x,y) = map Marked [x,x,y,y]

finestraD = map UnMarked . doppieI 2 4 4 
finestraS = map UnMarked . singoleI 2 2 2 

finestraDC (x,y) = telaioFinestra (x,y) ++ finestraD (x,y)
finestraSC (x,y) = telaioFinestra (x,y) ++ finestraS (x,y)
finestraTC (x,y) = telaioFinestra (x,y) ++ [BiMarked x] ++ finestraD (x,2 * y `div` 3)

telaioTrapezio (x1,x2,y) = map Marked [x1,x2,y,terzolato y $ x1 - x2]
telaioTriangolo (x,y) = map Marked [x,y,terzolato x y]
-------------------------------
--- porte 
--------------------------------

telaioPorta (x,y) = map Marked [x,x,y]

portaS = map UnMarked . singoleI 1 2 3
portaD  = map UnMarked . doppieI 1 4 6 

portaSC (x,y) = telaioPorta (x,y) ++ portaS (x,y)
portaDC (x,y) = telaioPorta (x,y) ++ portaD (x,y)

---------------------------------------------------------------------------

