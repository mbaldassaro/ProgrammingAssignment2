## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Computes inverse of matrix returned by makeCacheMatrix()

cacheSolve <- function(x, ...) {
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

##How to use:
##Source cachematrix.R into R environment
##Create matrix object, e.g.
##> a <- matrix(c(2,4,6,8),2,2))
##Pass var a as arg to makeCacheMatrix() and store it as a var b, e.g.
##> b <- makeCacheMatrix(a)
##Pass var b as arg to cacheSolve(), e.g.
##cacheSolve(b)
##Returns inverse of a matrix:
##        [,1]  [,2]
##[1, ]   -1.0  0.75
##[2, ]    0.5 -0.25
