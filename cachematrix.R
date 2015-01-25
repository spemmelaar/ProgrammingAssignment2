## The combined functions cacheSolve and makeCacheMatrix are used to calculate the
## inverse of a matrix and store the inverse matrix respectively. See more detail below.

## makeCacheMatrix stores a matrix and it's inverse value.
## It retuns a list of operations that include set and get (sets and gets a matrix) and
## setinverse and getinverse (sets and gets the inverse matrix).
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse = matrix()) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve accepts a makeCacheMatrix list as an argument and uses makeCacheMatrix
## operations to obtain a matrix and store that matrix's inverse after calculating it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
