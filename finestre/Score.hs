module Score where

import MBin

type Scarto a = MBin a -> a

mean :: (Num a,Integral a) => [a] -> Float
mean xs = sum (map fromIntegral xs) / fromIntegral (length xs)

dev :: (Integral a, Num a) => [a] -> Float
dev xs = sum (map (\x -> (fromIntegral x - mean xs) ^ 2) xs)/ fromIntegral (length xs) 

score :: (Integral a, Num a) => Scarto a -> Chromo a -> Float
score s c = dev . map s $ c


