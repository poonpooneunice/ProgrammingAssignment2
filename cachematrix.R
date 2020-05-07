# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
# of a matrix rather than computing it repeatedly.
# The following pair of functions - makeCacheMatrix() and cacheSolve() - are used to cache the inverse of a matrix

# Creates a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        # 1. set the value of the matrix    
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # 2. get the value of the matrix
        get <- function() x    
        # 3. set the value of the inverse
        setinverse <- function(inverse) i <<- inverse
        # 4. get the value of the inverse    
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

# Computes the inverse of the special 'matrix' returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        values <- x$get()
        i <- solve(values)
        x$setinverse(i)
        i
}
