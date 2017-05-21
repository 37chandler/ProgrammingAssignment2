# Matrix inversion is expensive. This code allows
# caching of matrix inversion to save running time.

# This function creates a list that is a 
# special type of matrix, allowing caching of
# its inverse. Built directly from makeVector
# example.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL # the inverse of x
  
  set <- function(y) { # defines x and inv
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x # just returns the matrix
  
  # sets (caches) the inverse matrix 
  setinv <- function(inverse.matrix) {
    inv <<- inverse.matrix
  }
  
  # returns the inverse
  getinv <- function() {
    inv
  }
  
  return(
    list(set = set, get = get,
       setinv = setinv,
       getinv = getinv))
}

# This function returns the inverse of
# a matrix. It first checks to see if the inverse
# has already been calculated, in which case it 
# uses the cached version.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("Getting cached inverse.")
    return(inv)
  }
  
  # We get here if there wasn't a pre-calculated inverse
  data <- x$get()

  inv <- solve(data,...)
  x$setinv(inv)
  inv
}

# Some testing.
xx <- matrix(rnorm(10000),nrow=100)
xxx <- makeCacheMatrix(xx)

c.inv <- cacheSolve(xxx)

# Quick test to see if we have an inverse
stopifnot(sum(diag(round(c.inv %*% xx)))==nrow(xx))

# Looks good. Let's do some timing to see if it worked.
# Set up a bigger matrix
xx <- matrix(rnorm(1000^2),nrow=1000)
xxx <- makeCacheMatrix(xx)

# First time through
system.time(
  c.inv <- cacheSolve(xxx) # 1.25 on my system
)

# Second time
system.time(
  c.inv <- cacheSolve(xxx) # instantaneous
)
