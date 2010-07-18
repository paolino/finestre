import Finestre
import MBin
import Breed
import Random
import Data.List (sort)
import Control.Monad

------------------------------------------------------------

scarto_3_4500 xs = 4400 - sum xs - (length xs -1) * 3
effScarto_3_4500 (u,n) = 4400 - u - n * 3 >= 0

tR :: Finestra
tR (x,y) = [x,x,y,y]
t4 x = replicate 4 x

doppia (x,y) = t4 (x-92) ++ t4 ((y-76) `div` 2)

singola (x,y) = tR (x - 92, y - 90)
doppiaC (x,y) = tR (x,y) ++ doppia (x,y)
singolaC (x,y) = tR (x,y) ++ singola (x,y)

telaio3 (x,y,z) = tR (x,y) ++ [x]
doppiaFC (x,y,z) = telaio3 (x,y,z) ++ doppia (x,z)
doppiaFCS (x,y) = doppiaFC (x,y,y `div` 3)

telaioTrapezio (x1,x2,y) = [x1,x2,y,floor . sqrt $ (fromIntegral y ^ 2 +
	fromIntegral (x1 - x2) ^ 2)] ++ [y]

trapezioDC (x1,x2,y) = telaioTrapezio (x1,x2,y) ++ doppia (min x1 x2, y)
-----------------------------
nord = singolaC (1200,700) ++ trapezioDC (860,1420,1430) ++ doppiaFCS (440,1490)
sud = concatMap doppiaFCS [(750,1780),(730,1880),(725,1560),(720,2095)]

wn = telaioTrapezio (1265,1950,1713) ++ [1713,1193,1033] ++ doppia (1193,1033)

ww = telaioTrapezio (1950,1537,1020) ++ [1020,1020] ++ doppia (1193,1020)
ws = telaioTrapezio (732,1410,1691) ++ [1691,657,1020] ++ doppia (657,1020)

ovest = wn ++ ww ++ ws

sottotetto = nord ++ sud ++ ovest

------------------------------------------------------------------------------

bagno = telaioTrapezio (345,1356,2317) ++ [893,958] ++ doppia (893,958) 
	++ telaioTrapezio (1130,890,480)  ++ singola (890,480)

------------------------
tutto = sottotetto ++ bagno


mkChromo = map (fromBin . mkBin) . firstFit scarto_3_4500 


-- scartoMedio_3_4500 :: [MBin] -> Misura
-- scartoMedio_3_4500 xs = map (\(MBin (l,n) _) -> l 
