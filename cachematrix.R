## Put comments here that give an overall description of what your
	## functions do
	

	## The following function creates a special matrix whose inverse can 
	## be cached
	

	makeCacheMatrix <- function(x = matrix()) {
	    inv <- NULL
	    set <- function(y){
	        x <<-y
	        inv <<- NULL
	    }
	    get <- function() x
	    setinv <- function(inverse) inv <<- inverse
	    getinv <- function() inv
	    list(set = set, get = get, 
	        setinv = setinv, getinv = getinv)
	}
	

	

	## The following function returns a matrix that is the inverse of 'x'
	## If the inverse is already present, then it is returned from the cache
	## Otherwise inverse is computed
	

	cacheSolve <- function(x, ...) { # x is a special matrix
	    inv <- x$getinv()
	    if(!is.null(inv)) {
	        message("inverse retrieved from the cache")
	        return (inv)
	    }        
	    mat <- x$get()
	    inv <- solve(mat, ...)
	    x$setinv(inv)
	    inv
	}

