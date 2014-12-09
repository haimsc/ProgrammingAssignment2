## caclculate the inverse of matrix, optimized with a cache of previously calculated results

## creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## calculates the inverse of the special "matrix" created with makeCacheMatrix. If a cached
## inverse exists, it returnes the cached value. Otherwise it caclcultes the inverse and
## puts the result in the cache
cacheSolve <- function(x, ...) {    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        ## use the cached inverse
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    inv
}
