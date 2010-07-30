module Random where

import System.Random
import Data.List (splitAt, delete,sortBy)
import Control.Monad
import Data.Ord (comparing)
import Control.Arrow



pick :: Maybe Int -> [a] -> IO (a,[a],Int)
pick ml xs = do
	let l = maybe (length xs) id ml - 1
	n <- randomRIO (0, l)
	let (ys,z:zs) = splitAt n xs
	return $ (z,ys ++ zs,l)

sample :: Int -> [a] -> IO [a]
sample n xs = fmap (\(_,rs,_) -> rs) . (\f -> foldM f (xs,[],Nothing) [1..n]) $ \(xs,rs,ml) _ -> do
	(x,ys,ml') <- pick ml xs
	return (ys,x:rs,Just ml')


wsample :: (Random t, Ord t, Num t) =>   
	[(t,a)]	-- ^ list of weighted elements (weight,elem)
        -> IO [(t,a)]        -- ^ picked elements
wsample xs = (map j . randomRs (0, sum . map fst $ xs)) `fmap` newStdGen
	where
	j x = snd . head. dropWhile ((< x). fst). scanl1 (\(s, _) (z, a) -> (s + z, a)) $ map (fst &&& id) xs

		
tackle :: (a -> a) ->  [a] -> IO [a]
tackle f xs = do	
	(x,xs',_) <- pick Nothing xs 
	return $ f x : xs'
	

pickF :: [a] -> IO a
pickF = fmap (\(x,_,_) -> x) . pick Nothing

wshuffle :: (Num t,Ord t, Random t) => [(t,a)] -> IO [a]
wshuffle xs = map snd `fmap` sortBy (flip $ comparing fst) `fmap` mapM (\(t,a) -> flip (,) a `fmap` randomRIO (0,t)) xs

shuffle :: [a] -> IO [a]
shuffle = wshuffle . map  ((,) (1 :: Float))
