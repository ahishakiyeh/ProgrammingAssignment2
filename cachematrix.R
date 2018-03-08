## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix Create a special "matrix" object that is able to cache its inverse.
## Write a short comment describing this function
# The function returns A list containing four functions to set and get the value of the
#  matrix and to set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

## Write a short comment describing this function
# cacheSolve computes the matrix inverse returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
## verification by calculate the inverse of a known matrix
x = rbind(c(1, 3), c(2, 4))
m = makeCacheMatrix(x)
m$get()
cacheSolve(m)
