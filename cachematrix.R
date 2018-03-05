## Creates a special matrix object that caches its inverse
## Inverse can be found with \code{cacheSolve(z)}.
## R script created with assistance from other github users



makeCacheMatrix <- function(x = matrix()) {
     z <- NULL
     # Defines the function to set the value of the matrix
     set <- function(y) {
          x <<- y    # Sets the value
          z <<- NULL # Clears the cache
     }
     # Define function to get the value of the matrix
     get <- function() x
     # Define function before setting the inverse
     setInverse <- function(inverse) m <<- inverse
     # Define before getting the inverse
     getInverse <- function() z
     
     # Returns a list with the 4 functions
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


#' Return inverse of matrix x
#' 
#' Calculates the inverse of the special "matrix" from makeCacheMatrix above. 
#' If the inverse has already been calculated (and the matrix has not changed), 
#' then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x) {
     z <- x$getInverse() # This fetches the cached value for the inverse
     if(!is.null(z)) { # returns inverse value if the cache is not empty
          message("getting cached data")
          return(z)
     }
     # Needs to recalculate and return the value
     data <- x$get()  # Gets the value of the matrix
     z <- solve(data) # Calculates inverse
     x$setInverse(z)  # Caches the result
     z                # Return the inverse value
}