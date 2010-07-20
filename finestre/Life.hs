module Life where

import Control.Monad


type Sample m a = Int -> [a] -> m [a]

type Breed m a = a -> a -> m a

passo :: (Monad m, Functor m) => Sample m a -> Breed m a -> Int -> [a] -> m [a]
passo s b n xs = fmap (++ xs) . replicateM n $ do
	[x,y] <- s 2 xs
	b x y
	

