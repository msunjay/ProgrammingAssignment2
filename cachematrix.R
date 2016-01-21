## Caching the inverse  of a matrix. Since Matrix Inversion is a costly computation, 
## there will be some benefit to caching the inverser of the matrix rahter than having to recalculate everytime.
## The functions below are being used to create a special object that stores a matrix and caches its inverse values.

## This Function creates a new "modified" matrix object that should be able to cache the matrix's inverse values.

makeCacheMatrix <- function(z = matrix()) {
      inv = NULL
      set <- function(y) {
            z <<- y
      }
      get <- function() z
      setInv <- function(inverse) inv <<-inverse
      getInv <- function() inv
      list(set = set,
           get = get,
           setInv = setInv,
           getInv = getInv)
}


## This function computes the inverse of the special "matrix"  that was created with "makeCacheMatrix" object above. 
## This function states that if m is not null it will return the result from the cached data. However if m is null,
## the function will get that data, inverse it and then return value of m after the setter has been established. 
Write a short comment describing this function

cacheSolve <- function(z, ...) {
      m <- z$getInv()
      if(!is.null(m)){
            message("Getting Cached Data")
            return(m)
      }
      data <- z$get()
      m <- solve(data, ...)
      z$set(m)
      m
}
