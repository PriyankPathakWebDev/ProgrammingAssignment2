## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## the function below creates a closure around the 
## passed matrix so that it can be Cached

makeCacheMatrix <- function(x = matrix()) {

Inv <- NULL
set <- function(y) {
        x <<- y
        Inv <<- NULL
}
get <- function() x
setInverse <- function(Inverse) Inv <<- Inverse
getInverse <- function() Inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)



}


## Write a short comment describing this function
## The function below uses the functions created above to 
## to solve for inverse and cache it as well

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}
