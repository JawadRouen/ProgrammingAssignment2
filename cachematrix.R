## the functions below create a variable 'makeCacheMatrix' that will first receive a matrix values at its creation
## and the inverse of this same matrix using 'cashSolve'.
## the aim of these functions is to use the cache when possible, in order to avoir a costly operation such as 'solve'.


## this function creates a list of functions that give us read and write access to a matrix and its inverse 
## via get and set for the matrix values 
## via getsolve and setsolve for the matrix's inverse values

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


## This function returns the matrix's inverse that is stored in the cache of 'x' ('x' is a makeCacheMatrix variable)
## However, if no inverse stored in the cache, the function computes the inverse of the matrix
## and sets the value in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    print(s)
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
