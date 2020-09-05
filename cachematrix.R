## This function makes a special"matrix" object 
## which cache its inverse

## The result is a list to:
## 1. set the matrix value, 2. get the matrix value
## 3. set the inverse matrix, 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The result of this function is the inverse of the special
## "matrix" returned by 'makeCacheMatrix'. This function will obtain
## the inverse from the cache if it has already been calculated
## and the matrix has not changed.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
