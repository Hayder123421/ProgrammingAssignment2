# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the value of the matrix
    set <- function(y) {
        x <<- y  # Assign the new matrix value to x in the parent environment
        inv <<- NULL  # Reset the cached inverse to NULL as the matrix has changed
    }
    
    # Function to get the value of the matrix
    get <- function() x  # Simply return the matrix x
    
    # Function to set the inverse of the matrix in the cache
    setInverse <- function(inverse) inv <<- inverse  # Assign the inverse to inv in the parent environment
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv  # Return the cached inverse, if it exists
    
    # Return a list of the functions to interact with the cached matrix and inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the matrix returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix has not changed, then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse, if it exists
    
    # Check if the inverse is already cached
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)  # Return the cached inverse
    }
    
    # If the inverse is not cached, compute it
    mat <- x$get()  # Get the original matrix
    inv <- solve(mat, ...)  # Compute the inverse using the solve function
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse
}
