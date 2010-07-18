module Breed where

import MBin
import Control.Monad
import Data.List (partition)
import Data.IntMap hiding (insert,partition)


surplus :: Chromo -> Question -> [Misura]
surplus z q = concatMap (\(k,x) -> replicate k x) . assocs $ difference (pieces z) q

missings :: Chromo -> Question -> [Misura]
missings z q = concatMap (\(k,x) -> replicate k x) .assocs $ difference q (pieces z)

type Shuffle m a = [a] -> m [a]
type TackleOne m a = (a -> a) -> [a] -> m [a]

purgeChromo :: (Functor m , Monad m) => TackleOne m MBin -> Question -> Chromo -> m Chromo
purgeChromo s q c = foldM f c $ surplus c q where
	f c x = let (gs,bs) = partition (contains x) c	in 
		case gs of 
			[] -> error "incoerenza"
			gs' -> (++ bs) `fmap` s (purge x) gs'	

updateChromo :: (Functor m, Monad m) 
	=> ((Misura,Int) -> Bool) ->  TackleOne m MBin 
	-> Question -> Chromo -> m Chromo
updateChromo k s q c = foldM f c $ missings c q where
	f c x = let (gs,bs) = partition (accept k x) c	in 
		case gs of 
			[] -> return $ MBin (x,1) (singleton x 1):bs
			gs -> (++ bs) `fmap` s (insert x) gs	

breed :: (Functor m, Monad m) 
	=> ((Misura,Int) -> Bool) ->  TackleOne m MBin 
	-> Shuffle m MBin -> Question -> Chromo -> Chromo -> m Chromo
breed k t s q x y = 
	take (length x) `fmap` s (x ++ y) >>= 
	purgeChromo t q >>= 
	updateChromo k t q 
	
