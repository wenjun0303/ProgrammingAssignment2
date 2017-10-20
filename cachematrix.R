## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## This function creates a special "matrix" object
## that can cache its inverse.
      inverse <- NULL
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      get <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)      

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has
## already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
      Inverse <- x$getInverse()
      if(!is.null(Inverse)){
            message("getting cached matrix")
            return(Inverse)
      }
      matrix <- x$get()
      Inverse <- solve(matrix,...)
      x$setInverse(Inverse)
      Inverse
}

## The following shows the test result:
##
## > matrix_test <- makeCacheMatrix(matrix(1:4,2,2))
##
## > matrix_test$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > matrix_test$getInverse()
## NULL
##
## > cacheSolve(matrix_test)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(matrix_test)
## getting cached matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > matrix_test$getInverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5