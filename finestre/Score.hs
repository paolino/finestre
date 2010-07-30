module Score where

import Breed (Chromo)
import MBin (MBin(..))
import Data.List (minimumBy)
import Data.Ord (comparing)

type Scarto a = MBin a -> a

mean :: (Num a,Integral a) => [a] -> Float
mean xs = sum (map fromIntegral xs) / fromIntegral (length xs)

dev :: (Integral a, Num a) => [a] -> Float
dev xs = sqrt $ sum (map (\x -> (fromIntegral x - mean xs) ^ 2) xs)/ fromIntegral (length xs) 

score :: (Integral a, Num a) => Chromo a -> Float
score c = dev . map somma $ c


