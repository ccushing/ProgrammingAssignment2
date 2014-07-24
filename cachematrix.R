## Put comments here that give an overall description of what your
## functions do

## This pair of functions allow for the caching and retrieval of the inversion of a giving matrix

## Write a short comment describing this function
## The makeCacheMatrix function creates a matrix which enables the caching and retrieving to get 
## and set the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {

    ## Store the value of the inverse in m
  m <- NULL
  
  ## define a set function to store y in the cache
  ## reset the inverse (m) to null if y changes
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) m <<- inv
  
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function will solve the inverse of the matrix x. It will first check to see if the result has already been
## cached. If it has, then it will return the cached version if not, it will calculate the inverse, return it, and 
## store the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Variable m stores the inverse of the matrix
  m <- x$getinverse()
  
  ## Check to see if the cache already exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
  
}




