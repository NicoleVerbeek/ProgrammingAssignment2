## As matrix inversion is usually a costly computation, there may be some benefit 
## in caching the inverse, rather than computing it repeatedly.
## This script provides the functions makeCacheMatrix and solveCache which will
## be explained below. 
## Finally, I have included a trial run to show how the function work.

## makeCacheMatrix creates a list that contains a function that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
 invs <- NULL
 set <- function(y) {
   x <<- y
   inv <<- NULL
 }
 get <- function() {x}
 setinverse <- function(inverse) {invs <<- inverse}
 getinverse <- function() {invs}
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of a cached matrix. 
## First it checks whether the inverse is already calculated.  
## If the inverse is already calculated, it immediately displays the inverse.
## If there is no inverse calculated yet, it calculates the inverse and displays it.
## Keep in mind that the function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}
## example of a trial run:
## z <- rbind(c(1, 2, 3), c(2, -3, 4), c(1, 2, -3))
## w <- makeCacheMatrix(z)
## w$get()
## cacheSolve(w)
## cacheSolve(w)