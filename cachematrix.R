## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Method the get the matrix
  get <- function() x ##Returns the matrix
  setInverse <- function(inverse) i <<- inverse ##Set the matrix
  getInverse <- function() i  ##Get the inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  ## Check if inverse is already set
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Get the matrix
  x <- x$get()
  ## Calculate Inverse
  inv <- solve(x, ...)
  ## Set the inverse
  x$setInverse(i)
  ## Return the inverse
  i
}
