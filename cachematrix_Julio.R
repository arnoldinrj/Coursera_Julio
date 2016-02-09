@@ -4,12 +4,32 @@
## The first function is responsible for storing 
## the matrix that will be cached for another function 

  
  makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }



## This function will use the cached data to 
## define if is necessary or not to calculate the inverse matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}