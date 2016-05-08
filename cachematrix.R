## This file contains 2 functions:
## 1) makeCacheMatrix
## 2) cacheSolve

## Function (1) makeCacheMatrix: Creates a special "matrix" 
## object which caches its inverse.
makeCacheMatrix <- function(x = matrix()) {  ## Argument has default mode of "matrix"
  m_inv <- NULL                              ## Initialize m_inv as NULL; holds inverse matrix values
  set <- function(y) {                       ## Define the set function (set the value of the matrix)
    x <<- y
    m_inv <<- NULL                           ## Reset m_inv to NULL (if there is new matrix)
  }
  
  get <- function() x                        ## Define the get funtion (get the value of the matrix)
  setinverse <- function(inverse) m_inv <<- inverse ## Sets the m_inv value in parent environment
  getinverse <- function() m_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function (2) cacheSolve: Computes the inverse of the special 
## "matrix" returned by the previous 'makeCacheMatrix' function
cacheSolve <- function(x, ...) {           ## Return a matrix that is the inverse of 'x'
  m_inv <- x$getinverse()                  ## If the inverse matrix is not present, return message
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)                ## Calculate the inverted matrix
  x$setinverse(m_inv)
  m_inv
}
  