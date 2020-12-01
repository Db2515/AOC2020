multOnSum xs sum = [x*y | x <- xs, y <-xs, x+y == sum]
