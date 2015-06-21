## [Put comments here that describe what your functions do]

makeCacheMatrix <- function(x = matrix()) {
    ## x is a square invertible matrix
  
  inv = NULL
  
    ## sets the value of inv to NULL
  
  set = function(y) {
    x <<- y
    ## use `<<-` to assign a value to an object in an environment different from the current environment. 
    
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## x is the output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  
  if (!is.null(inv)){ 
    
    ## if the inverse has already been calculated get it from the cache and skip the computation.
    
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise, calculates the inverse and set the value of the inverse in the cache via the setinv function.
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
}
