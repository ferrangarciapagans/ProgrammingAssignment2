## This is my Solution to the Peer Assigment
## R Programing course on Coursera
## This file contains functions makeCacheMatrix and cacheSolve

## Test it with:
## B 
## newMatrix <- makeCacheMatrix(B)
## cacheSolve(newMatrix)
## 1st time the system will compute the inverse matrix and will cache it
## cacheSolve(newMatrix)
## Now the system will return the cached matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getsolve()
  if(!is.null(m)) {
    ## Return cached data
    message("getting cached data")
    return(m)
  }
  ## Compute inversr matrix and cache it
  message("Calculating the inverse matrix")
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
