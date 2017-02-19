## Caching the Inverse of a Matrix:
## Matrix inversion is usually a time-consumming process and there may be some 
## benefit to caching the inverse of them rather than doing it repeatedly.
## these are functions used to create a special object that stores matrices 
## and caches their inverses.
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(solve) inv <<- inverse
		getinv <- function() inv
		list(set = set, get = get, 
			setinv = setinv,
			getinv =getinv)
}


## This function computes the inverse of the special "matrix" 
## created by the fucntion above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should get back the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		x <- x$getinv()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setinv(inv)
		inv
}

