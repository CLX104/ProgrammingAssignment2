## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    internalStore <-NULL
    set <- function(mat) {  
       x <<- mat                  ##Store the matrix
       internalStore <<- NULL
    }
    
    setinverse <- function(inv) internalStore <<- inv  ##Store inverse
    getInverse <- function() internalStore 
    
    get <- function() x

    list(set = set, get = get,
             setinverse = setinverse,
             getInverse = getInverse )
    
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

     inv <- x$getInverse()
     if(!is.null(inv)) {
                message("returning cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data) %*% data
        x$setinverse(inv)
        inv
   
}
