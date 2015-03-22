## Course : Coursera - R Programming
## Programming Assignment 2


################################################################################
# As suggeste below makeCacheMatrix creates a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of inverse of the matrix
# get the value of inverse of the matrix
#################################################################################

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_x <<-inverse
  getinverse <- function() inverse_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
################################################################################ 
# As suggested below cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
#################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$getinverse()
  if (!is.null(inverse_x)) {
    message("getting cached inverse matrix")
    return(inverse_x)
  } else {
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}
