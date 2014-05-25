## Put comments here that give an overall description of what your
## functions do

## Creates a cache matrix (which can be used via the cache Solve function)
makeCacheMatrix <- function(x = matrix()) {
    ## initialization
    inv <- NULL
    ## if the matrix is changed delete the cached inverse
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    ##defining get and set functions
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    ##create the function list
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## Returns the inverse matrix. By the first calculation of the inverse, the inverse is cached, so it is not needed to be calculated again (except the matrix itself is changed)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
    ## if there is a cached inverse, return the cached one.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ##otherwiese calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
