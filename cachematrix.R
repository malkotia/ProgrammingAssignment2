## This function creates an invertible matrix
## This matrix caches its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes matrix returned by makeCacheMatrix as the input
##It computes the inverse of the input matric 
## If the inverse is already computed and the matrix has not changed
## it will retrieve inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("fetching data from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m

}
