## Two functions that work together to: 
## First create a "matrix" object that is able to cache its inverse (makeCacheMatrix)
## Then calculate the inverse of the "matrix" object (cacheSolve)

## makeCacheMatrix creates a special "matrix" object 
## that will be able to cache its inverse

makeCacheMatrix <- function(x = Matrix()) {
    myMatrix <- NULL
    set <- function(y) {
        x <<- y
        myMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) myMatrix <<- inverse
    getInverse <- function() myInverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve returns the inverse of the "matrix" 
## only if it has already been calculated and the 
## "matrix" has not changed  

cacheSolve <- function(x, ...) {
    myMatrix <- x$getInverse()
    if(!is.null(myMatrix)) {
        message("getting cached data")
        return(myMatrix)
    }
    data <- x$get()
    myMatrix <- solve(x, ...)
    x$setInverse(myMatrix)
    myMatrix
}
