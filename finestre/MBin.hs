{-# LANGUAGE NoMonomorphismRestriction #-}
module MBin where
import Prelude hiding (lookup)
import Data.Map hiding (map, partition)
import Data.List (group, sort)
import Control.Arrow ((&&&))
import qualified Data.Array as A
import Control.Parallel.Strategies
import Debug.Trace


type Bin a  = Map a Int

-- mkBin :: [Misura] -> Bin
mkBin = fromList . map (head &&& length) . group . sort 

data MBin a = MBin !a !Int (Bin a) deriving Show

-- fromBin  :: Bin -> MBin
fromBin bin = let (m,n) = foldr (\(x,m) (k,n) -> (fromIntegral m * x + k, n + 1)) (0,0) $ assocs bin
	in MBin m n bin
  
-- vuoto :: MBin 
vuoto = MBin 0 0 empty

-- contains :: Misura -> MBin -> Bool
contains x (MBin _ _ b) = case lookup x b of
	Nothing -> False
	Just r -> r > 0

-- purge :: Misura -> MBin -> MBin 
purge x (MBin g n b) =  MBin (g - x) (n -1) $ adjust (subtract 1) x b

-- accept :: (Misura -> Int -> Bool) -> Misura -> MBin -> Bool
accept k x (MBin g n _) = k (g + x) (n + 1) 

-- insert :: Misura -> MBin -> MBin 
insert x (MBin g n b) =  MBin (g + x) (n + 1) $ insertWith (+) x 1 b


-- sviluppo :: Bin -> [Misura]
sviluppo = concatMap (\(x,k) -> replicate k x) . assocs






