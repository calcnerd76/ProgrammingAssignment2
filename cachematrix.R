## These functions will allow us to cache the 
## inverse of a matrix in order to save computation time. 
## Rather than recomputing the inverse, 
## it will simply look up the cached value.

## The makeCacheMatrix function will create a special "matrix", 
## which is really a list containing functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function will output the inverse of a matrix. 
## If the inverse has already been previously calculated, 
## it will use the cache data. 
## Otherwise, it will compute the new inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}