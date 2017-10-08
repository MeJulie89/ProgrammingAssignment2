## The following functions create a special object that stores a matrix 
## and caches the inverse of this matrix. 

## makeCacheMatrix creates a special "matrix" - a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    j <- NULL
    set <- function(y) {
        x <<- y
        j <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) j <<- inverse
    getinverse <- function() j
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned above
## If the matrix does not change, and the inverse has been calculated before, 
## cacheSolve will retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getinverse()
  if (!is.null(j)) {
    message("getting cached data")
    return(j)
  }
  data <- x$get
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
