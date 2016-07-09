# the following two functions can be used to cache the inverse of a matrix, rather
# than computing it repeatedly which can become a costly operation

# the makeCacheMatrix function creates a special matrix object (really 
# a list of function calls) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  
  set = function(y) {
    x <<- y # <<- uses a diff env
    inv <<- NULL
  }
  
  get = function() x
  
  setInv = function(inverse) inv <<- inverse 
  
  getInv = function() inv
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


# the cacheSolve function computes the inverse of the special matrix returned by 
# the makeCacheMatrix above. If the inverse has already been calculated (and 
# the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  inv = x$getInv()
  
  # first check to see if the inverse is in cache
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  # if nothing in the cache, use the solve function to calculate the inverse
  matx = x$get()
  inv = solve(matx, ...)
  
  # set and return the value of the inverse
  x$setInv(inv)
  return(inv)

}
