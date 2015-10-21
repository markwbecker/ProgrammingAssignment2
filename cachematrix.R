## Coursera R Programming Class
## Programming Assignment 2: cached inverse matrix
## Mark Becker
## October 21, 2015

## Includes functions makeCacheMatrix(m) and cacheSolve(m)
## Assumes a square matrix as argument 
## Examples of testing these functions, showing the return of the 
#  cached matrix after the first call:

## test_m <- matrix(1:4, nrow=2, ncol=2)
##
## cache_m <- makeCacheMatrix(test_m)
## 
## cacheSolve(cache_m)
##
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## cacheSolve(cache_m)
## getting cached inverse matrix
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## inverse_m <- cacheSolve(cache_m)
## getting cached inverse matrix
## testm %*% inverse_m
##        [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## ########################################################################


## The makeCacheMatrix(m) function takes a rectangular (e.g. 2x2, 3x3) matrix 
## and 'decorates' it with a series of functions: get, set, getinverse, setinverse
## The new enhanced matrix is like an object-oriented class that has
## the origial matrix and an inverse of that matrix as members.

makeCacheMatrix <- function(m = matrix()) {
  inverse_m <- NULL
  
  set <- function(m_orig) {
      m <<- m_orig
      inverse_m <<- NULL
  }
  
  get <- function() { 
      m
  }
  
  setinverse <- function(inv) {
      inverse_m <<- inv
  }
  
  getinverse <- function() {
      inverse_m
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function takes as argument a 'decorated' matrix created
## by the makeCacheMatrix() function. It returns the cached inverse
## matrix if it exists; that is, the cached matrix will be returned every time
## after the initial call, in which the inverse matrix is calculated 
## for the first time

cacheSolve <- function(m, ...) {
        ## Return a matrim that is the inverse of 'm'
  inv <- m$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  
  data <- m$get()
  inv <- solve(data, ...)
  m$setinverse(inv)
  inv  
}
