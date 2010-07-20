import System.Random

sample m 1 x = return [x]
sample m n x =  do
	y <- randomRIO (m,x)
	let r = x - y
	if r <= m then 
		return [x]
		else do 
			ys <- sample m (n - 1) (x - y)
			return (y:ys) 
