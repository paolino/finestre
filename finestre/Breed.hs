module Breed where

import MBin
import Control.Monad
import Data.List (partition,sort)
import Data.Map hiding (insert,partition,filter)
import qualified Data.Map as M (map)
import Control.Arrow ((&&&))
import Debug.Trace

diff :: Ord t => Chromo t -> Question t -> [Either t t]
diff z q = let 	f (x,k) = replicate (abs k)  $ if k > 0 then Right x else Left x 
		in reverse . sort . concatMap f . assocs $ unionWith (+)  (pieces z) (M.map negate q)

type Shuffle m a = [a] -> m [a]
type TackleOne m a = (a -> a) -> [a] -> m [a]

-- correct :: (Ord t, Functor m , Monad m, Num t) => (t -> Int -> Bool) -> TackleOne m (MBin t) -> Question t -> Chromo t -> m (Chromo t)
correct k s q c = foldM f c $ diff c q where
	f c (Right x) = let (gs,bs) = partition (contains x) c	in 
		case gs of 
			[] -> error $ "incoerenza"
			gs' -> (++ bs) `fmap` s (purge x) gs'	
	f c (Left x) = let (gs,bs) = partition (accept k x) c	in 
		case gs of 
			[] -> return $ MBin x 1 (singleton x 1):bs
			gs' ->  (++ bs) `fmap` s (insert x) gs'	

-- breed :: (Ord t, Functor m, Monad m, Num t) 
-- 	=> (t -> Int -> Bool) ->  TackleOne m (MBin t)
-- 	-> Shuffle m (MBin t) -> Question t -> Int -> Chromo t -> Chromo t -> m (Chromo t)
breed k t s q v x y = take v `fmap` s (x ++ y) >>= correct k t q
	
	
