# This function generates list that have functions to
# Set value of matrix
# Get value of matrix
# Set inverse of matrix
# Get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse) m <<- inverse
  get.inverse <- function() m
  list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}

# Function to check if matrix is cached and return the inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	 m <- x$get.inverse()
  if(!is.null(m)) {
    message("inverse is returned from cached results")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat)
  x$set.inverse(m)
  m
}
