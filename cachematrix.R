## Put comments here that give an overall description of what your
## functions do
##The following function stores any matrix and caches its inverse 
##for future computing 

## Write a short comment describing this function
##The following function stores a matrix and calaculates the inverse and caches it

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL 
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get<-function() x
  setinv<-function(inverse) i<<-inverse
  getinv<- function() i
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}


## Write a short comment describing this function
##This matrix calculates the inverse of a matrix
##If the matrix and it's inverse were already cached then the inverse is printed 
##along with the message"getting cached data"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}


