## These functions are designed to cache the inverse of a matrix, Caching the inverse saves time by avoiding repeated calculations.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of four functions to:set and to get the value of the matrix,and the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  set <- function(y) {  
    x <<- y  # Assign the matrix to the parent environment
    inv <<- NULL  # Reset the cached inverse
  }
  get <- function() x  # Return the matrix
  setinverse <- function(inverse) inv <<- inverse  # Cache the inverse
  getinverse <- function() inv  # Return the cached inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix. 
## If the inverse has already been computed and cached, it retrieves the result from the cache. 
## Otherwise, it calculates the inverse and stores it in the cache.




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()  # Check if the inverse is already cached
  if(!is.null(inv)) {  # If cached, return the cached inverse
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()  # Get the matrix from the special object
  inv <- solve(data, ...)  # Compute the inverse of the matrix
  x$setinverse(inv)  # Cache the computed inverse
  inv  # Return the computed inverse
}
