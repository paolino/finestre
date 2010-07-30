{-# LANGUAGE NoMonomorphismRestriction #-}

import Infissi
import Driver
import Score 


telaioInterno = map BiMarked
------------------------------------------------------
--- sottotetto
-------------------------------------------------

-- N
u1 = finestraSC (1200,700)
u2 = finestraTC (1200,1950)
u3 = telaioTrapezio (500,1077,1437) ++ finestraD (500,1437) ++ telaioInterno [1437]
u4 = finestraTC (440,1490)
-- S
u5 = finestraTC (750,1780)
u6 = finestraTC (730,1880)
u7 = finestraTC (725,1560)
u8 = finestraTC (720,2095)
-- W
u9 = telaioTrapezio (705,1369,1665) ++ telaioInterno [1665,649,1004] ++ finestraD (649,1004)
u10 = telaioTrapezio (1901,1501,1004) ++ telaioInterno [1004,1004] ++ finestraD (1181,1004)
u11 = telaioTrapezio (1231,1924,1687) ++ telaioInterno [1687,1181,1011] ++ finestraD (1181,1011)

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

b1 = telaioTrapezio (890,1130,480) ++ telaioInterno [480] ++ finestraS (890,480)
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
tutto = sottotetto
-- bedroom ++ disimpegno ++ corridoio ++ living ++ sottotetto ++ bagno
 
------------------------------------
lunghezzaListe = 4250
scartoTaglio = 3

main = let 
	(main,_,_) =  mkDriver scartoTaglio lunghezzaListe  (map misura tutto) (trace (+ scartoTaglio) lunghezzaListe tutto) score
	in main
