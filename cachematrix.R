## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x) {
  m <- NULL
  set <- function(x) {
    y <<- x
    m <<- NULL
  }
  get <- function() y
  setinv <- function(inv) {
    inv <<- solve(inv)
    m <<- inv
  }
  getinv <- function() m
  list(set = set(x), get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
