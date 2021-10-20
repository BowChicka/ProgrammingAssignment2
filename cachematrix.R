## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function which allows the user to create a "matrix" object in which its inverse will be calculated and then cached

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function() a
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

## cacheSolve computes the matrix returned by the previous function, if the inverse is done already the already calculated inverse will be retrieved from the cache

cacheSolve <- function(x, ...) {
  a <- x$getInverse()
  if(!is.null(a)) {
    message("getting inversed matrix")
    return(a)
  }
  data <- x$get()
  a <- solve(data, ...)
  x$setInverse(a)
  a
}