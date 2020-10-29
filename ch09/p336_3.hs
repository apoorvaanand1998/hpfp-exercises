mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTupleList = length $ [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
