## These functions pre-allocate the data and then calculate the inverse of a
  ## square matrix.


## The function "makeCacheMatrix" creates a random matrix that will pre-allocate
  ## the data of the inverse matrix.
  ## The attribute "x" is a square invertible matrix.

# example:
#matriz <- matrix(c(rnorm(9)), nrow=3, byrow=TRUE)

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

# example:
#pre_aloc <- makeCacheMatrix(matriz)


## The function "cacheSolve" calculates the inverse of a square matrix.
  ## The attribute "x" is the list created using the "makeCacheMatrix" function.


cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data") ####
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s

}

# example:
#cacheSolve(pre_aloc)
