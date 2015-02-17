### Introduction

Matrix inversion is usually a costly computation and there may be some
benefit to caching the inverse of a matrix rather than computing it
repeatedly (there are also alternatives to matrix inversion that we will
not discuss here).

### Caching the Inverse of a Matrix

Below are two functions that are used to create a
special object that stores a matrix and caches its inverse.

The first function, `makeCacheMatrix` creates a special "matrix", which is
really a list containing a function to

1.  set the value of the matrix
2.  get the value of the matrix
3.  set the value of the inverse
4.  get the value of the inverse

<!-- -->

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        # assign the functions
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        
        # assemble the list of functions
        list(set = set,
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

The following function calculates the inverse of the special "matrix"
created with the above function. However, it first checks to see if the
inverse has already been calculated. If so, it `get`s the inverse from the
cache and skips the computation. Otherwise, it calculates the inverse of
the matrix and sets the value of the inverse in the cache via the `setinverse`
function.

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
