{-# LANGUAGE ScopedTypeVariables #-}
module Life where

import Control.Monad
import Control.Arrow 

type Tempo = Int

type Pick m a = [a] -> m a

type Breed m a = a -> a -> m a

passo :: forall m a . (Eq a, Monad m, Functor m) => Pick m a -> Breed m a -> Int -> [a] -> m [a]
passo s b n xs = do
	
	let	single = do
			x <- s xs
			y <- s xs 
			b x y
	(++ xs) `fmap` replicateM n single 
	
	
	

