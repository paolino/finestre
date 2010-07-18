module Random where

import System.Random
import Data.List (splitAt)

shuffle :: [a] -> IO [a]
shuffle = shuffle' Nothing 

shuffle' :: Maybe Int -> [a] -> IO [a]
shuffle' _  [] = error "empty"
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

tackle :: (a -> a) ->  [a] -> IO [a]
tackle f xs = do	
	(x,xs',_) <- pick Nothing xs 
	return $ f x : xs'
	

pickF :: [a] -> IO a
pickF = fmap (\(x,_,_) -> x) . pick Nothing
