## Put comments here that give an overall description of what your
## functions do : set the value of the matrix,get the value of the matrix
## set the value of the inverse, get the value of the inverse, 
## then used the array without modifying it, find 
## the inverse of the original matrix


## Write a short comment describing this function
## set the value of the matrix,get the value of the matrix
## set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## Write a short comment describing this function
## used the array without modifying it, find 
## the inverse of the original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
