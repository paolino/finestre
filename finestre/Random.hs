module Random where

import System.Random
import Data.List (splitAt)
import Control.Monad


shuffle :: [a] -> IO [a]
shuffle = shuffle' Nothing 

shuffle' :: Maybe Int -> [a] -> IO [a]
shuffle' _  [] = return []
shuffle' _ [x] = return [x]
shuffle' ml xs = do 
	(x,xs',l) <- pick ml xs
	xs'' <- shuffle' (Just l) xs'
	return $ x : xs''

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

wsample ::   
	[(Float,a)]	-- ^ list of weighted elements (weight,elem)
        -> IO [a]        -- ^ picked elements
wsample xs = (map j . randomRs (0, sum . map fst $ xs)) `fmap` getStdGen
	where
	j x = snd. head. dropWhile ((< x). fst). scanl1 (\(s, _) (z, a) -> (s + z, a)) $ xs

		
tackle :: (a -> a) ->  [a] -> IO [a]
tackle f xs = do	
	(x,xs',_) <- pick Nothing xs 
	return $ f x : xs'
	

pickF :: [a] -> IO a
pickF = fmap (\(x,_,_) -> x) . pick Nothing
