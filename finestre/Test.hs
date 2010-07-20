{-# LANGUAGE NoMonomorphismRestriction #-}
import Finestre
import MBin
import Breed
import Random
import Life
import Score
import Data.List (mapAccumL, sort, minimumBy, sortBy, intercalate,find,delete)
import Control.Monad
import System.Random
import Data.Ord (comparing) 
import Control.Arrow
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Text.Printf

type Misura = Int 

data Marked 
	= BiMarked {misura :: Misura} 
	| Marked {misura :: Misura} 
	| UnMarked  {misura :: Misura} deriving (Show,Eq)

markmap f (BiMarked x) = BiMarked (f x)
markmap f (Marked x) = Marked (f x)
markmap f (UnMarked x) = UnMarked (f x)

kx = 46
kys = 88
kyd = 74

t = replicate 
singoleI n t1 t2 (x,y) = t t1 (x - kx * n) ++ t t2 (y - kys) 
doppieI n t1 t2 (x,y) = t t1 (x - kx * n) ++ t t2 ((y - kyd) `div` 2)

---------------------------------------------------------------------
--- finestre ............
---------------------------------------------

telaioFinestra (x,y) = map Marked [x,x,y,y]

finestraD = map UnMarked . doppieI 2 4 4 
finestraS = map UnMarked . singoleI 2 2 2 

finestraDC (x,y) = telaioFinestra (x,y) ++ finestraD (x,y)
finestraSC (x,y) = telaioFinestra (x,y) ++ finestraS (x,y)
finestraTC (x,y) = telaioFinestra (x,y) ++ [BiMarked x] ++ finestraD (x,2 * y `div` 3)

telaioTrapezio (x1,x2,y) = map Marked [x1,x2,y,floor . sqrt $ (fromIntegral y ^ 2 + fromIntegral (x1 - x2) ^ 2)]
telaioTriangolo (x,y) = tail $ telaioTrapezio (0,x,y)
-------------------------------
--- porte 
--------------------------------

telaioPorta (x,y) = map Marked [x,x,y]

portaS = map UnMarked . singoleI 1 2 3
portaD  = map UnMarked . doppieI 1 4 6 

portaSC (x,y) = telaioPorta (x,y) ++ portaS (x,y)
portaDC (x,y) = telaioPorta (x,y) ++ portaD (x,y)

---------------------------------------------------------------------------
--- sottotetto
-------------------------------------------------

u1 = finestraSC (1200,700)
u2 = finestraTC (1200,1950)
u3 = telaioTrapezio (860,1420,1430) ++ finestraD (860,1430) ++ [BiMarked 1430]
u4 = finestraTC (440,1490)
u5 = finestraTC (750,1780)
u6 = finestraTC (730,1880)
u7 = finestraTC (725,1560)
u8 = finestraTC (720,2095)
u9 = telaioTrapezio (732,1410,1691) ++ map BiMarked [1691,657,1020] ++ finestraD (657,1020)
u10 = telaioTrapezio (1950,1537,1020) ++ map BiMarked [1020,1020] ++ finestraD (1193,1020)
u11 = telaioTrapezio (1265,1950,1713) ++ map BiMarked [1713,1193,1033] ++ finestraD (1193,1033)

sottotetto = concat [u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11]
----------------------------------------------------------------------------
-- living
------------------------------------------------------------------------------

l1 = portaSC (1820,680)
l2 = portaDC (2430,980)
l3 = finestraSC (1010,720)
l4 = l3
l5 = finestraDC (1210,860)
l6 = finestraSC (1260,810)

living = concat [l3,l4,l5,l6]

-----------------------------
--- bagno
------------------------

b1 = telaioTrapezio (890,1130,480) ++ [BiMarked 480] ++ finestraS (890,480)
b2 = telaioTriangolo (1196,2332)
b3 = finestraSC (605,500)

bagno = concat [b1,b2,b3]

---------------------
-- corridoio
------------------

c1 = finestraSC (840,590)
c2 = finestraSC (820,660)

corridoio = c1 ++ c2

---------------------
-- disimpegno
--------------------

d1 = portaDC (1945,940)
d2 = finestraDC (1425,820)
d3 = portaSC (1850,800)

disimpegno = concat [d2]
----------------------
-- bedroom
--------------------

br1 = finestraDC (1320,850)
br2 = portaSC (2000,680)
br3 = portaSC (1990, 600)

bedroom = concat [br1]

-------------------------------

tutto :: [Marked]
tutto = bedroom ++ disimpegno ++ corridoio ++ living ++ sottotetto ++ bagno


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
scarto_3_4500 :: [Misura] -> Misura
scarto_3_4500 xs = 4400 - sum xs - (length xs -1) * 3

scartoBin_3_4500 :: MBin Misura -> Misura
scartoBin_3_4500 (MBin u n _) = 4400 - u - (n -1) * 3

effScarto_3_4500 :: Misura -> Int -> Bool
effScarto_3_4500 u n = 4400 - u - (n - 1) * 3 >= 0

------------------------------------------------------------------------



misure = map misura tutto

valid :: Chromo Misura -> Bool
valid xs = pieces xs == pieces (mkChromo misure)

limit :: Int
limit = (sum misure + fromIntegral (3 * length misure)) `div` 4400

mkChromo :: [Misura] -> Chromo Misura
mkChromo = map (fromBin . mkBin) . firstFit scarto_3_4500 

scarto :: Chromo Misura -> Misura
scarto = sum . map scartoBin_3_4500  

population :: Int -> IO [Chromo Misura]
population n = map mkChromo `fmap` replicateM n (shuffle misure)

breedo :: Int -> Chromo Misura -> Chromo Misura -> IO (Chromo Misura)
breedo z x y = breed effScarto_3_4500 tackle shuffle (mkBin misure) (limit - z) x y

-- scoro :: MBin Misura -> Misura
scoro = score scartoBin_3_4500

tronco k  = map snd . take k . sortBy (flip $ comparing fst) . map (scoro &&& id) 

taglio3 :: [Marked] -> [(Marked,Marked)]
taglio3 = snd . mapAccumL (\t x -> (t + misura x + 3, ((t +) `markmap` x,x))) 0 .  reverse . sortBy (flip $ comparing misura)
	
showMisura x = printf "%3d,%1d" a b
	where (a,b) =  x `divMod` 10 

showTagli :: [[(Marked,Marked)]] -> String
showTagli  = unlines . map (\(n,ys) -> printf "%2d)  "  n ++  
	intercalate " " (map (\(x,_) -> showMark x) ys ) ++ replicate (max 0 ((7 - length ys) * 7)) ' ' ++ " (" ++ 
	intercalate " " (map (\(_,t) -> showMark t) ys )++ ")") . zip [(1 :: Int)..]
	
steppo z k xs = do
	let 	f x y = case compare (length x) (length y) of
			LT -> LT
			GT -> GT
			EQ -> compare (scoro y) (scoro x)
		xs' = take k $ sortBy f xs
	print $ (length $ head xs', minimumBy (comparing $ \(MBin x _ _) -> x) $ head xs')
	writeFile "soluzione" $ showTagli . map taglio3 . marks tutto $ head xs'
	passo sample (breedo z) k xs' >>= steppo z k


main = do 
	(n:z:ys) <- getArgs
	xs <- population (read n)
	steppo (read z) (read n) xs	
