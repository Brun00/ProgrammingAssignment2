
## Two functions that allow to calculate and cache an invert of a matrix


## Creates a list object storing a matrix and it's inverse
##input - a matrix
##output - a list containing the matrix and it's inverse (or NULL if inverse wasn't created)

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
    
}


## Calculates inverse of a matrix stored by makeCacheMatrix or reads it from makeCacheMatrix if it was previously created
## outputs the inversed matrix "im"

cacheSolve <- function(x, ...) {
    im <- x$getinverse() ##retrives a inverse matrix or NULL value 
    if(!is.null(im)) {
        message("getting cached inverse of the input matrix")
        return(im)
    }
    data <- x$get()
    im <- solve(data) ## sets im variable to be an inverse of the input matrix
    x$setinverse(im)
    im ## Return a matrix that is the inverse of 'x'
}
