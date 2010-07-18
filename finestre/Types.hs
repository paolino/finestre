module MBin where

import Data.IntMap hiding (map)

type Misura = Int

type Bin = IntMap Misura Int

data MBin = MBin (Misura,Int) Bin

vuoto :: MBin 
vuoto = MBin (0,0) empty

purge' :: Misura -> Bin -> Maybe Bin
purge' x b = case findWithDefault 0 x b of
	0 -> Nothing
	_ -> Just (adjust (subtract 1) x b)

purge :: Misura -> MBin -> MBin 
purge x (MBin (g,n) b) =  MBin (g - x, n -1) `fmap` purge' x b 

accept :: ((Misura,Int) -> Bool) -> Misura -> MBin -> Bool
accept k x (MBin (g,n) _) = k (g + x , n + 1) 

insert' :: Misura -> Bin -> Bin 
insert' x b = adjust (+ 1) x b

insert :: Misura -> MBin -> MBin 
insert x (MBin (g,n) b) = MBin (g + x, n + 1) $ insert' x b

type Chromo = [MBin]
type Question = Bin

pieces :: Chromo -> Bin
pieces = unions . map (\(MBin _ x) -> x)



