## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache for the inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cache when a new matrix is assigned
  }
  
  get <- function() x  # Return the stored matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Store the inverse
  
  getInverse <- function() inv  # Retrieve the cached inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)  # Return cached inverse if available
  }
  
  data <- x$get()  # Retrieve the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the inverse
}

# ---- TEST ----
# Step 1: Create a matrix
m <- matrix(c(1, 2, 3, 4), 2, 2)

# Step 2: Create a cached matrix object
cm <- makeCacheMatrix(m)

# Step 3: Compute the inverse
cacheSolve(cm)
