## Matrix inversion is a costly computational operation.
## Hence, we implement a caching solution where the inverse 
## of a matrix is cached (temporarily stored in the working environment)
## so that its automatically recalled from memory whenver we attempt
## to compute the inverse of a particular more than once.
## This implementation takes advantage of the lexical scoping of the R langugage.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
  matrix_inv <- NULL
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inv <<- inverse
  getinverse <- function() matrix_inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}	



## Return a matrix that is the inverse of matrix 'x'

cacheSolve <- function(x, ...) {
 
  matrix_inv <- x$getinverse()
  if(!is.null(matrix_inv)) {
    message("Getting the cached data.")
    return(matrix_inv)
  }
  inverse_matrix_data <- x$get()
  matrix_inv <- solve(inverse_matrix_data)
  x$setinverse(matrix_inv)
  matrix_inv
}
