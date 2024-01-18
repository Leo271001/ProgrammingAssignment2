# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  inverse <- NULL
  
  set <- function(new_matrix) {
    mat <<- new_matrix
    inverse <<- NULL
  }
  
  get <- function() mat
  
  setInverse <- function(new_inverse) inverse <<- new_inverse
  
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache the result
cacheSolve <- function(cacheMatrix) {
  
  mat <- cacheMatrix$get()
  
  if (!is.null(cacheMatrix$getInverse())) {
    message("Getting cached data.")
    return(cacheMatrix$getInverse())
  }
  
  inverse <- solve(mat)
  
  cacheMatrix$setInverse(inverse)
  
  inverse
}
