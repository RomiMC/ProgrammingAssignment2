## These functions take a matrix x and wrap it into an object that caches the
## inverse of x

## The makeCacheMatrix fuction takes a matrix x and wrap it in an object.
## Data: x the input matrix
##       m  the inverse of x
##          m is intialize to NULL and get the inverse value of x
##          when cacheSolve is called
## Functions: set : intializes x and m
##            get : return x
##            setinverse : set the inverse value of x in m
##            getinverse : return m:se
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function gets a makeCacheMatrix object 'x' and returns
## its inverse value. At the first time cacheSolve is called it is calling the
## solve function, store the value in the makeCacheMatrix object and returns
## the inverse value. On the successive calls it retrieves the
## value from the makeCacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
