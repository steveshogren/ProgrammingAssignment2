## Provides a memoized value along with the data structure
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmemo <- function(memo) m <<- memo
  getmemo <- function() m
  list(set = set, get = get,
       setmemo = setmemo,
       getmemo = getmemo)
}


## inverses a cacheMatrix, otherwise returning
## the memoized value
cacheSolve <- function(x, ...) {
  m <- x$getmemo()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
