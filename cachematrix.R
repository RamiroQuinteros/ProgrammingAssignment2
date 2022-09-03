## "makeCacheMatrix" and "cacheSolve" are functions that allow caching a matrix and 
## get the inverse. This functions, avoid calculating the inverse more than once.

##Sample usage:
## matriz<-matrix(1:4,2,2)
## x<-makeCacheMatrix(matriz)
## cacheSolve(x)


## makeCacheMatrix() creates a cacheable matrix, uses 4 differents functions
## to set and get the values of the matrix, and the inverse of the matrix.
## The matrix can be accessed via '$get()' and changed via '$set()'.
## The inverse can be accessed via '$getinverse()' and changed via '$setinverse()'.

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<- function(y) {
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) i<<-inverse
  getinverse <- function() {i}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() uses solve() function to get the inverse of cacheable matrix, and
## sets the value of the inverse in the cache via the setinverse function.
## If the inverse of the matrix has already been calculated, cacheSolve() gets 
## the inverse from the cache and skips the computation.


cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}