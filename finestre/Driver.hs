{-# LANGUAGE NoMonomorphismRestriction #-}
module Driver (mkDriver) where

import MBin
import Breed
import Random
import Life
import Score
import Data.List (mapAccumL, sort, minimumBy, sortBy, intercalate,find,delete)
import Control.Monad
import System.Random
import Data.Ord (comparing) 
import Control.Arrow
import System.Environment
import Control.Parallel
import Control.Parallel.Strategies
import Text.Printf

type Misura = Int

mkDriver cutWaste maxWood ms report = let 
	scarto 	:: Misura -> Int -> Misura
	scarto u n = maxWood - u - (fromIntegral $ n - 1) * cutWaste
	
	limit :: Misura
	limit = (sum ms + fromIntegral (cutWaste * length ms)) `div` maxWood

	breedo :: Chromo Misura -> Chromo Misura -> IO (Chromo Misura)
	breedo x y = breed (\x y -> scarto x y >= 0) tackle shuffle (mkBin ms) limit x y

	steppo 	:: Int
		-> [Chromo Misura]
		-> IO ()
	steppo k xs = do
		let 	scartoBin (MBin u n _) = scarto u n
		 	f x y = case compare (length x) (length y) of
				LT -> LT
				GT -> GT
				EQ -> compare (score scartoBin y) (score scartoBin x)
			xs' = take k $ sortBy f xs
		report xs' >> passo sample breedo k xs' >>= steppo k

	driverMain :: IO ()
	driverMain = do 
		(n:ys) <- getArgs
		xs <- replicateM (read n) $ breedo [] [] 
		steppo (read n) xs	
	in (driverMain,breedo,steppo) 
