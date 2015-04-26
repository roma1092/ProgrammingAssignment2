## Two functions that calculate and cache the inverse of a matrix

## Caches the inverse of the matrix

makeCacheMatrix <- function(x = matrix())
  {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solvedMatrix) m <<- solvedMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

# Checks for cached matrix, then solves for inverse

cacheSolve <- function(x, ...)
  {
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
