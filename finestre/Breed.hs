{-# LANGUAGE Rank2Types #-}
module Breed (breed,Chromo) where

import MBin (MBin (..), purge, accept, insert, Bin, contains)
import Control.Monad
import Data.List (partition,sort,sortBy)
import Data.Map hiding (insert,partition,filter,map)
import qualified Data.Map as M (map)
import Control.Arrow ((&&&))
import System.Random (Random)
import Debug.Trace

type Chromo a = [MBin a]
type Question a = Bin a

pieces :: Ord t =>  Chromo t -> Bin t
pieces = unionsWith (+) . map (\(MBin _ _ x) -> x)

diff :: (Ord t, Num t) => Chromo t -> Question t -> ([t],[t])
diff z q = let 	(ts,ps) = partition ((> 0) . snd) . assocs $ unionWith (+)  (pieces z) (M.map negate q)
		mult  = concatMap (\(x,k) -> replicate (abs k) x)
	in (mult ts, mult ps)
			 
type Shuffle m t a = [(t,a)] -> m [a]
type TackleOne m a = (a -> a) -> [a] -> m [a]

-- | random first fit 
correct :: (Ord t, Functor m , Monad m, Num t, Random t) 
	=> (t -> Int -> Bool) 
	-> Shuffle m t t
	-> TackleOne m (MBin t) 
	-> Question t 
	-> Chromo t 
	-> m (Chromo t)

correct k s t q c = do
	let (ts,ps) = diff c q  -- i problemi del chromo (doppioni e mancanti)
	c' <- foldM f c ts -- elimina i doppioni
-- 	ps' <- s $ map (id &&& id) ps
	let ps' = sortBy (flip compare) ps-- firstFitDecreasing con i mancanti
	foldM g c' ps' 	
	where
	f c x = let (gs,bs) = partition (contains x) c	in 
		case gs of 
			[] -> error $ "incoerenza"
			gs' -> (++ bs) `fmap` t (purge x) gs'	
	g c x = let (gs,bs) = partition (accept k x) c	in 
		case gs of 
			[] -> return $ MBin x 1 (singleton x 1):bs
			gs' ->  (++ bs) `fmap` t (insert x) gs'

breed :: (Ord t, Functor m, Monad m, Num t, Random t, Integral t) 
 	=> (t -> Int -> Bool)  	-- ^ dati i pezzi e il numero di tagli esprime la accettabilita'
	-> TackleOne m (MBin t)	-- ^ modificatore random degli elementi
 	-> (forall a t .(Ord t , Num t, Random t) => Shuffle m t a)	-- ^ rimescolatore di elementi
	-> Question t 		-- ^ pezzi
	-> Int 			-- ^ elementi iniziali per il figlio
	-> Chromo t 		-- ^ madre
	-> Chromo t 		-- ^ padre
	-> m (Chromo t)		-- ^ figlio
breed k t s q v x y = take v `fmap` s (map (const (100::Float)  &&& id) (x ++ y)) >>= correct k s t q
	
	
