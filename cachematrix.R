## This is created for Caching the inverse of a matrix 
## These matrixes are used to create objects that store store
## matrix and caches inverse

## This function has a matrix that can cache inverse

makeCacheMatrix <- function(x = matrix()) {
  
    inverse <- NULL
    set1 <- function(y) {
            x <<- y
            inv <<- NULL
    }
    getnow <- function() x
    myinverse <- function(inverse) inv <<- inverse
    grabinverse <- function() inv
    list(set1 = set1,
         getnow = getnow,
         myinverse = myinverse,
         grabinverse = grabinverse)

}


## This computes the inverse of maceCachMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inverse)){
              message("here is cached data")
              return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix,...)
        x$setInverse(inverse)
        inverse
}
