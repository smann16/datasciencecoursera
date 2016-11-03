# Name: cachematrix.R
# Date: November 02, 2016
# The following functions are used to cache the inverse S of a matrix A in
# order to avoid recomputing S unless the original matrix A has changed.

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(A = matrix()){
	M <- NULL
	set <- function(B)
	{
		A <<- B
		M <<- NULL
	}
	get <- function() A
	setinv <- function(B) M <<- B
	getinv <- function() M
	list(set = set, get = get, 
	     setinv = setinv, getinv = getinv
           )
}


# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.

cacheSolve <- function(A, ...){
	M <- A$getinv()
	if (is.null(M)) {
		message('Computing cache data...')
		 data <- A$get()
		 M <- solve(data, ...)
		 A$setinv(M)
	} else {
		message('Skipping computation...')
	}
	M
}
		
		
# Run example:
		
> source("cachematrix.R")
> a <- makeCacheMatrix()
> set.seed(1)
> a$set(matrix(runif(16,-1,1),4))
> cacheSolve(a)
Computing cache data...
           [,1]       [,2]       [,3]       [,4]
[1,] -0.4870633 -0.7016570 0.04233537  0.7177189
[2,] -1.2877322 -0.2121515 0.79330739 -0.9477680
[3,] -1.2611511 -0.9871923 0.44060295 -1.1123421
[4,]  0.8793920 -0.5366228 1.01429950  0.1560319
> cacheSolve(a)
Skipping computation...
           [,1]       [,2]       [,3]       [,4]
[1,] -0.4870633 -0.7016570 0.04233537  0.7177189
[2,] -1.2877322 -0.2121515 0.79330739 -0.9477680
[3,] -1.2611511 -0.9871923 0.44060295 -1.1123421
[4,]  0.8793920 -0.5366228 1.01429950  0.1560319
> a$set(matrix(runif(16,-1,1),4))
> cacheSolve(a)
Computing cache data...
           [,1]        [,2]        [,3]       [,4]
[1,] 0.45558692  0.90240093 -0.35458476 -0.3084514
[2,] 0.47501645  0.40810606 -0.05233992 -1.1187837
[3,] 0.01601109 -0.05671709 -0.94446805 -0.3203795
[4,] 0.53582817 -1.04678694 -0.32442242  1.2953745
> 

