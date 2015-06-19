## Provides a generic memoize ability that
## can be used to append any memoized value to
## the data structure
makeCacheMatrix <- function(x = matrix()) {
  m <- list()
  set <- function(y) {
    x <<- y
    m <<- list()
  }
  get <- function() x
  setmemo <- function(key, val) m[[key]] <<- val
  getmemo <- function(key) m[[key]]
  list(set = set, get = get,
       setmemo = setmemo,
       getmemo = getmemo)
}


## inverses a cacheMatrix, otherwise returning
## the memoized value
cacheSolve <- function(x, ...) {
  ## look for the "solve" cached value
  m <- x$getmemo("solve")
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m <- solve(x$get(), ...)
  x$setmemo("solve", m)
  m
}
