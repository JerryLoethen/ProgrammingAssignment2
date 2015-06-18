#################################################################################
## 
## R Programming class, Programming Assignement 2 - Lexical Scopeing
## Functions: makeCacheMatrix will create a matrix object with three functions.
## 	      cacheSolve will return the inverse of the matrix used in the call
##	     	to makeCacheMatrix.
##
#################################################################################

#################################################################################
## 
## Function: makeCacheMatrix
## Description:	Returns an object based on an input matrix that will calculate and
##		store in memory the inverse of the input matrix.
## Usage: m <- makeCacheMatrix(x) where x is a valid, invertable matrix. Must be used
##		with cacheSolve().
##        Inverse_m <- cacheSolve(m) where m is the result of makeCacheMatrix()
##
##	The object resulting from the makeCahceMatrix() call will have three functions:
##		set(newMatrix) will replace the original matrix with a new one.
##		get() returns the currently stored matrix.
##		getinverse() returns the inverse of the currently stored matrix from
##			either the cache or by recomputing it if needed.
##		
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
	cacheInverse_x <- NULL

	set <- function(newMatrix) {
			x <<- newMatrix
			cacheInverse_x <<- NULL
		}
	get <- function() x
	## setinverse <- function {} Not needed because getinverse also sets it.
	getinverse <- function() {
			## Thanks to George Kousis on the class forum for this suggestion
			## https://class.coursera.org/rprog-015/forum/thread?thread_id=364
			if(!is.null(cacheInverse_x)) { 
				message("getting cached data")
				return(cacheInverse_x)
			}
			cacheInverse_x <<- solve(get())
			return(cacheInverse_x)
		} 

	list (set = set, 
	      get = get,
              getinverse = getinverse)
}


#################################################################################
##
## Function: cacheSolve
## Description:	Returns the inverse of the matrix used in the call to makeCacheMatrix().
##		Becuase of the changes to getinverse() in makeCacheMatrix this
##		function is not really needed and the object is now mostly self-contained.
## Usage: m <- makeCacheMatrix(x) where x is a valid, invertable matrix. Must be used
##		with cacheSolve().
##        Inverse_m <- cacheSolve(m) where m is the result of makeCacheMatrix()
##
#################################################################################

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse_x <- x$getinverse()
}

