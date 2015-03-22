## Assignment: Caching the Inverse of a Matrix
##
#############################
## Functions:
##
## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## cacheSolve:      Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##
## Example of use:
##
##
## clear
## 
## source("cachematrix.R") # load functions
## 
## matk <- rbind(c(3,3.5),c(3.2,3.6)) # create a matrix
## 
## l <- makeCacheMatrix(matk)             # execute caching function
## 
## cacheSolve(l)                      # calculate inverse of matrix (and cache) since first-time round
## cacheSolve(l)                      # retrieve inverse from cache, since already cached
## 
## matj <- rbind(c(4,7),c(2,6))     # define a different input matrix
## 
## l$set(matj)                        # set-up it up as new input matrix
## cacheSolve(l)                      # calculate inverse of matrix (and cache) since first-time round
## cacheSolve(l)                      # retrieve inverse from cache, since already cached


#############################
## Function: makeCacheMatrix: 
##
## Input(s):    a matrix
## Output(s):   a list of functions which can operate on the input matrix
## Description: 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#############################
## Function: cacheSolve: 
##
## Input(s):    special matrix produced by above function
## Output(s):   a list of functions which can operate on the input matrix
## Description: Returns a matrix that is the inverse of 'x':
##  - by calculating inverse and caching if not already done
##  - by returning cached value if already cached

cacheSolve <- function(x, ...) {

  ## Check to see if inverse of 'x' already cached, if so, return cached matrix 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## If not already cached, retreive 'x',
  data <- x$get()
  ##  calculate inverse,
  m <- solve(data)
  ##  cache results,
  x$setinverse(m)
  ## return results (inverse of 'x')
  m
}