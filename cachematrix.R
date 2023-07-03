## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function takes a matrix as input and returns a list of functions that can be used to set the value of the matrix, get the value of the matrix, set the value of the inverse, and get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
# This function takes as input a “matrix” object created by makeCacheMatrix and checks if the inverse has already been calculated and stored in the cache. If it has, it retrieves the inverse from the cache and returns it. Otherwise, it calculates the inverse using the solve function, stores it in the cache using the setInverse function, and returns it.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

