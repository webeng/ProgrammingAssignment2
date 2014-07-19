## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. It has a matrix x as an input
## there are also 4 other function within makeCacheMatrix that are used to manipulate the matrix x
## function "set" assigns a new value y to x and set m to empty == null
## function "get" returns the current matrix
## function "setsolve" has a variable "solve" as a parameter and assigns it to  m
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
## hilbert is a squared with entries being the unit fractions 1/n
## hilbert(n) creates a n x n matrix with unit fraction in each cell i.e: m[0,0] = 1/1, m[0,1] = 1/2, m[0,2] = 1/3 and so on
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

m <- makeCacheMatrix(hilbert(8))
cacheSolve(m)
cacheSolve(m)