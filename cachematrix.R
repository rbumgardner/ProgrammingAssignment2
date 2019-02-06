## These functions demonstrate a cacheable inverse of a 
## given matrix. If the inverse does not exist in the 
## "cache", it will be calculated and stored in the 
## "cache".

## makeCacheMatrix creates an object that contains
## a matrix and it's inverse. The object also
## contains a set of methods (functions) that allow
## the "getting" and "setting" of both the matrix
## and the inverse (encapsulation).

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(new_matrix) {
    x <<- new_matrix
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(new_inverse) {
    inverse <<- new_inverse
  }
  
  getinverse <- function() {
    inverse
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns an inverse of a given matrix.
## It will either fetch the inverse of the matrix
## from the cache or it will calculate the inverse
## and store it in the cache. This function takes
## a makeCacheMatrix object as an argument.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  } else {
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv)
    return(inv)
  }
}
