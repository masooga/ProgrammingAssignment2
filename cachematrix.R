## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
z <- matrix(2:5, 2, 2)
makeCacheMatrix <- function(x = matrix()) {
##This function creates a special "matrix" object that can cache its inverse.
     n <- NULL
     get <- function() x
     cacheSolve <- function(solve) n <<- solve()
     cacheGet <- function() n
     list(get = get, cacheSolve = cacheSolve, cacheGet = cacheGet)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ##From the README: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then the `cachesolve` should retrieve the inverse from the cache.
     n <- x$cacheGet()
     print(n)
     if(!is.null(n)) {
          message("This was already cached!")
          return(n)
     } else {
          print("We had to solve this one")
          data <- x$get()
          n <- solve(data, ...)
          x$cacheSolve(n)
          n
     }
}


##Example code from README for reference
makeVector <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setmean <- function(mean) m <<- mean
     getmean <- function() m
     list(set = set, get = get,
          setmean = setmean,
          getmean = getmean)
}

cachemean <- function(x, ...) {
     m <- x$getmean()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- mean(data, ...)
     x$setmean(m)
     m
}