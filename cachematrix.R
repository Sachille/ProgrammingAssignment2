## Keep a matrix and it's inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  if(is.null(dim(data))){
    message("cached data is not a matrix")
  } else if(dim(data)[1]==dim(data)[2]){
    message("cached matrix is square")
  } else{
    message("cached matrix is not square")
  }
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
