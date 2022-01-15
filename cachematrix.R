## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the value of the vector
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##get the value of the vector
  get <- function() {x}
  ## set value of inverse
  setinverse <- function(inverse) {m <<- inverse}
  ## get value of inverse
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    ##inverse is retrieved from cache
    message("getting cached data")
    return(m)
  }
  ##else,compute the inverse of the matrix.
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
}
