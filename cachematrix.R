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