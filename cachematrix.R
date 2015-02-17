## Matrix inverse calculation.
## Returns a cached result for the inverse matrix if it is already calculated.
## Calculates, caches and returns the inverse matrix if the value was not
## already present in the cache.

# Implements a cacheable "matrix" that caches the result of the matrix inverse
# operation.  The cacheable matrix consists of a list of functions that:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # assign the functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        # assemble the list of functions
        list(set = set,
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

# Calculates the inverse of a matrix.
# Prior to performing the inverse operation the cache is checked.  If a value
# is present in the cache this pre-computed value is returned.  Otherwise
# the inverse is calculated and the inverse is cached prior to being returned.
#
# The function assumes that teh matrix is always invertible.
cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check the cache
        m <- x$getinverse()
        if (!is.null(m)) {
                # inverse matrix is present in the cache so return the value
                return(m)
        }
        
        # retrieve the underlying matrix
        matrix <- x$get()
        # calculate the inverse matrix
        inv <- solve(matrix, ...)
        # cache the inverse matrix
        x$setinverse(inv)
        # return the inverse matrix
        inv
}
