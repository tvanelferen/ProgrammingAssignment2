## This R.programm contains two main-functions and does two things: 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## and cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Created by Tobias van Elferen (NL) for the Coursera Course "R-programming" (rprog-032)

## ---------------------------------

## This is the "makeCacheMatrix" part. It creates an matrix, then caches it's inverse. 
## It does that in 4 steps: set and get the value of the vector, 
## then set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      ## set the value of the vector
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      ## get the value of the vector
      get <- function() x
      ## set the value of the inverse
      setInverse <- function(solve) m <<- solve
      ## get the value of the inverse
      getInverse <- function() m
      
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## ---------------------------------

## This part computes the inverse of the special "matrix". It takes it from the cache,
## created by "MakeCacheMatrix" and when that is not-available, creates it instantly.

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
      ## Check if cached data is available... 
      if(!is.null(m)) {
            message("getting cached data")
           ## return it when available
             return(m)
      }
      ## when inverse-matrix isn't available, compute it on the spot
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      ## Return a matrix that is the inverse of 'x'
      m
        
}
