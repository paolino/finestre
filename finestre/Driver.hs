{-# LANGUAGE NoMonomorphismRestriction #-}
module Driver (mkDriver) where

import MBin
import Breed
import Random
import Life
import Score
import Algorithms
import Data.List (mapAccumL, sort, minimumBy, sortBy,group, intercalate,find,delete)
import Control.Monad
import System.Random
import Data.Ord (comparing) 
import Data.Function
import Control.Arrow
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Text.Printf

type Misura = Int
type Tempo = Int

mkDriver cutWaste maxWood ms report score = let 
	scarto 	:: Misura -> Int -> Misura
	scarto u n = maxWood - u - (fromIntegral $ n - 1) * cutWaste
	
	limit :: Misura
	limit = (sum ms + fromIntegral (cutWaste * length ms)) `div` maxWood

	breedo :: Chromo Misura -> Chromo Misura -> IO (Chromo Misura)
	breedo x y = breed (\x y -> scarto x y >= 0) tackle wshuffle (mkBin ms) limit x y

	steppo 	:: Int 
		-> [Chromo Misura]
		-> IO [Chromo Misura]
	steppo k xs = do
		report xs
		xss <- passo (fmap head . shuffle) breedo k xs
		let 	f x y = case compare (length x) (length y) of
				LT -> LT
				GT -> GT
				EQ -> compare (score y) (score x)
		return $ take k $ map head . group . sortBy f $ xss 

	newChromo :: IO (Chromo Misura)
	newChromo  = map (fromBin . mkBin) `fmap` firstFit (\xs -> scarto (sum xs) (length xs)) `fmap` shuffle ms 
	driverMain :: IO ()
	driverMain = do 
		(n:ys) <- getArgs
		xs <- replicateM (read n) newChromo
		foldM (const . steppo (read n)) xs [1..]	
		return ()
	in (driverMain,breedo,steppo) 
