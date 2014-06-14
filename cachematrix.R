#
# Coursera: R Programming, by Roger D. Peng
#
# Programming Assignment 2: Lexical Scoping
#
# File: cachematrix.R
#

#
# The makeCacheMatrix function creates a special "matrix" object 
# that can cache its inverse, which is really a list 
# containing a function to:
# 
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
#

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(cacheSolve) m <<- cacheSolve
  getinv <- function() m
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


#
# The function cacheSolve calculates the mean of the special "matrix" 
# created with the makeCacheMatrix function. 
#
# It first checks to see if the inverse of the matrix has already been 
# calculated.
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value 
# of the inverse in the cache via the setinv function.
#

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  
  m
}
