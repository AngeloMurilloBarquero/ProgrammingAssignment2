## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { # Creates a matrix object that can cache the inverse of the input
  inv <- NULL
  set <- function(y) {  #This option set the value of the matrix
    x <<- y          # To assigns a value based on a function, we are working in a second level    
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) (inv <<- inverse)
  getinverse <- function() (inv)
  list(set = set, get = get, setinverse = setinverse ,getinverse = getinverse)
  
}


cacheSolve <- function(x, ...) { #Computes the inverse of the matrix
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}


