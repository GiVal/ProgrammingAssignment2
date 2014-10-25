## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matr = matrix()) {
        inv <- NULL
        set <- function(y) {
                matr <<- y
                inv  <<- NULL
        }
        get <- function() matr
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(matr, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- matr$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- matr$get()
        inv <- solve(data, ...)
        matr$setinverse(inv)
        inv
}
