module MBin where
import Prelude hiding (lookup)
import Data.IntMap hiding (map, partition)
import Data.List (group, sort)
import Control.Arrow ((&&&))

type Misura = Int

type Bin = IntMap Int

mkBin :: [Misura] -> Bin
mkBin = fromList . map (head &&& length) . group . sort 


type Esecuzione = (Misura,Int) 

data MBin = MBin Esecuzione Bin deriving Show

fromBin  :: Bin -> MBin
fromBin bin = MBin (foldr (\(x,m) (k,n) -> (m * x + k, n + 1)) (0,0) $ assocs bin) bin
  
vuoto :: MBin 
vuoto = MBin (0,0) empty

contains :: Misura -> MBin -> Bool
contains x (MBin _ b) = case lookup x b of
	Nothing -> False
	Just r -> r > 0

purge :: Misura -> MBin -> MBin 
purge x (MBin (g,n) b) =  MBin (g - x, n -1) $ adjust (subtract 1) x b

accept :: (Esecuzione -> Bool) -> Misura -> MBin -> Bool
accept k x (MBin (g,n) _) = k (g + x , n + 1) 

insert :: Misura -> MBin -> MBin 
insert x (MBin (g,n) b) = MBin (g + x, n + 1) $ adjust (+ 1) x b

type Chromo = [MBin]
type Question = Bin

pieces :: Chromo -> Bin
pieces = unions . map (\(MBin _ x) -> x)



