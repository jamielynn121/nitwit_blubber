## makeCacheMatrix creates a matrix 'x'
## creates a variable inv set to NULL
## creates 'set' a function of 'y'
## where 'y' is 'x'and 'inv' is NULL
## creates a function 'get' for 'x'
## creates 'setmatrixinv' to the solve function
## assigns the solved function to the 'inv'
## creates 'getmatrixinv' to return the 'inv' inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) inv <<- solve
  getmatrixinv <- function() inv
  list(set = set, get = get,
       setmatrixinv = setmatrixinv, 
       getmatrixinv = getmatrixinv)
}


## cacheSolve computes the inverse of makeCacheMatrix or
## retrieves the inverse if it has already been solved

cacheSolve <- function(x, ...) {
  inv <- x$getmatrixinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setmatrixinv(inv)
  inv
## 'inv returns a matrix that is the inverse of 'x'
}
