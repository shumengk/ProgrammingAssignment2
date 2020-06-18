## Caching the Inverse of a Matrix
## 

## makeCacheMatrix creates a specoal matrix that can cache its inverse
## The special matrix is a list containing 
## a function to perform the following: 
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of the inverse
## (4) get the value of the inverse 
## Similar to makeVector() in README.md

makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
      x <<- y
      mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mat <<- inverse
    getinverse <- function() mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## cacheSolve() solves the inverse of the matrix created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
  mat <- x$getinverse()
  if(!is.null(mat) {
    return(mat)
  }  
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
