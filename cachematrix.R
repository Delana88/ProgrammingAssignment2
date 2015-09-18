## The function creates a special object (matrix) that can cache its inverse
##  to save time in further computations which require the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  setMatrix <- function(y) x <<- y
  getMatrix <- function() x
  
  xInv <- NULL
  
  setInvertedMatrix <- function(y) xInv <<- y
  getInvertedMatrix <- function() xInv
  
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInvertedMatrix=setInvertedMatrix, getInvertedMatrix=getInvertedMatrix)
  
}

## Calculates the inverted matrix if not already cached.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInvertedMatrix()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$getMatrix()
  inv <- solve(data)
  x$setInvertedMatrix(inv)
  inv
}
