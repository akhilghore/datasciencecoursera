
#makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.
#cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). 
#If the inverse has already been calculated and the matrix has not changed, 
#it'll retrieves the inverse from the cache directly.


# Creating a makeCacheMatrix object will consist of
# four functions encapsulated in a list
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse of the matrix
# 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {

  # Initially set to NULL
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function returns the inverse of the matrix
# If the inverse has already been computed. it gets the result and skips the
# computation. If not, it computes the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
