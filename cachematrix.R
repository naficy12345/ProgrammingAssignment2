## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly.
## Here, a pair of functions that cache the inverse of a matrix is developed.



## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x is an input parameter (an invertible matrix)
  ## makeCacheMatrix returns a list containing the following functions: 
  ## set: set the value of the metrix
  ## get: get the value of the metrix
  ## setinverse: set the value of the inverse
  ## getinverse: get the value of the inverse
  ## The above list is used as input to the cacheSolve() function
  
  inv = NULL
  set = function(y) {
    ## <<- operator is used to assign a value to an object in an environment that is different from the current environment
    x <<- y
    inv <<- NULL      
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  
  inv = x$getinverse()
  if (!is.null(inv)){
    ## If the inverse has already been calculated (and the matrix has not changed), 
    ## then the cachesolve should retrieve the inverse from the cache and skips the computation.
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, cachesolve calculates the inverse 
  mat.data = x$get()
  ## Computing the inverse of a square matrix can be done with the solve function in R. 
  ## If X is a square invertible matrix, then solve(X) returns its inverse.
  inv = solve(mat.data, ...)
  x$setinverse(inv)
  inv
}
