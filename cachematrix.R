
## The makeCacheMatrix function caches the value of
## the inverse of a matrix in an invisible list.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  invisible(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## The cacheSolve function returns the value of the 
## inverse matrix computed with makeCacheMatrix or
## calculates it from the data.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
