## makeCacheMatrix (from assignment)
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  setcachematrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  getmatrix <- function() x
  
  computeinverse <- function(solve) m <<- solve
  
  getcomputedinverse <- function() m

  list(setcachematrix = setcachematrix, 
       getmatrix = getmatrix,
       computeinverse = computeinverse,
       getcomputedinverse = getcomputedinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getcomputedinverse()
  if(!is.null(m)) {
    message("getting computed inverse")
    return(m)
  }

  data <- x$getmatrix()
  m <- x$computeinverse(data)
  x$setcachematix(m)

}
