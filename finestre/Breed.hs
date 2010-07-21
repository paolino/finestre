module Breed (breed,Chromo) where

import MBin (MBin (..), purge, accept, insert, Bin, contains)
import Control.Monad
import Data.List (partition,sort)
import Data.Map hiding (insert,partition,filter,map)
import qualified Data.Map as M (map)
import Control.Arrow ((&&&))
import Debug.Trace

type Chromo a = [MBin a]
type Question a = Bin a

pieces :: Ord t =>  Chromo t -> Bin t
pieces = unionsWith (+) . map (\(MBin _ _ x) -> x)

diff :: Ord t => Chromo t -> Question t -> [Either t t]
diff z q = let 	f (x,k) = replicate (abs k)  $ if k > 0 then Right x else Left x 
		in reverse . sort . concatMap f . assocs $ unionWith (+)  (pieces z) (M.map negate q)

type Shuffle m a = [a] -> m [a]
type TackleOne m a = (a -> a) -> [a] -> m [a]

-- | random first fit 
correct :: (Ord t, Functor m , Monad m, Num t) 
	=> (t -> Int -> Bool) 
	-> TackleOne m (MBin t) 
	-> Question t 
	-> Chromo t 
	-> m (Chromo t)
correct k s q c = foldM f c $ diff c q where
	f c (Right x) = let (gs,bs) = partition (contains x) c	in 
		case gs of 
			[] -> error $ "incoerenza"
			gs' -> (++ bs) `fmap` s (purge x) gs'	
	f c (Left x) = let (gs,bs) = partition (accept k x) c	in 
		case gs of 
			[] -> return $ MBin x 1 (singleton x 1):bs
			gs' ->  (++ bs) `fmap` s (insert x) gs'	

breed :: (Ord t, Functor m, Monad m, Num t) 
 	=> (t -> Int -> Bool)  	-- ^ dati i pezzi e il numero di tagli esprime la accettabilita'
	-> TackleOne m (MBin t)	-- ^ modificatore random degli elementi
 	-> Shuffle m (MBin t)	-- ^ rimescolatore degli elementi
	-> Question t 		-- ^ pezzi
	-> Int 			-- ^ elementi iniziali per il figlio
	-> Chromo t 		-- ^ madre
	-> Chromo t 		-- ^ padre
	-> m (Chromo t)		-- ^ figlio
breed k t s q v x y = take v `fmap` s (x ++ y) >>= correct k t q
	
	
