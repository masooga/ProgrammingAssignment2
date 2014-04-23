## These two functions work together to calculate and cache a matrix's inverse. 

## makeCacheMatrix is a function that turns a normal matrix into an object-like matrix with four functions: set, get, setInverse, getInverse. Get
## returns the original matrix, set allows you to rerun the function on a different matrix, and setInverse and getInverse calculate and 
## return the inverted matrix respectively

makeCacheMatrix <- function(x = matrix()) {
##This function creates a special "matrix" object that can cache its inverse.
     n <- NULL
     set <- function(y) {
          x <<- y
          n <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) n <<- solve
     getInverse <- function() n
     list(get = get, set = set,
          setInverse = setInverse,
          getInverse = getInverse)

}

## cacheSolve is a function that takes the new type of matrix created by makeCacheMatrix and calls the matrix's getInverse to see if the 
## inverse of the matrix has already been calculated and cached. If it is in the cache, cacheSolve prints the value. If it hasn't been cached, 
## cacheSolve calculates the inverse of the matrix itself and puts it in the cache. 

cacheSolve <- function(x, ...) {
     n <<- x$getInverse()
     if(!is.null(n)) {
          message("This was already cached!")
          return(n)
     } else {
          message("We had to solve this one")
          data <- x$get()
          n <<- solve(data, ...)
          x$setInverse(n)
          return(n)
     }
}