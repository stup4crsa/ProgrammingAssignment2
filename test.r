# to use / test:
m1 <- matrix(sample(1:25),5,5)
m2 <- matrix(sample(1:25),5,5)
mx <- makeCacheMatrix()
mx$set(m1)
cacheSolve(mx)
cacheSolve(mx)
mx$set(m2)
cacheSolve(mx)
cacheSolve(mx)