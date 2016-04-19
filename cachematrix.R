## Function creating a special "matrix" object that can cache its inverse

## Function calculates the inversion of a given matrix and caches it's results

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}


## Function computing the inverse of the special "matrix" returned by makeCacheMatrix

## Function first checks if chached inverion exists:
##  - yes - it returns it
##  - no - it computes it and stores the inversion

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

