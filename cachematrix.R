## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. It has a matrix x as an input
## there are also 4 other function within makeCacheMatrix that are used to manipulate the matrix x
## function "set" assigns a new value to x
## function "get" returns the current matrix
## function "setsolve" sets  m
## function "getsolve" returns m
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}
## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## First of all, we get the current value of m. If is not null it means that our result comes from the cache
## If is null, we get the current matrix and inverse it using the function solve. Then we store the inverted matrix
## in the cache using the function setsolve
cacheSolve <- function(x, ...) {

  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

m <- makeCacheMatrix(hilbert(8))
cacheSolve(m)
cacheSolve(m)