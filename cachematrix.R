## Put comments here that give an overall description of what your
## functions do
## cachematrix.r, Coursera, R.prgAssignment 2, kongmanes

## Write a short comment describing this function

##starts matrix as input, set matrix value, get matrix value, 
##set inverse then get the inverse. Cache matrix. 
makeCacheMatrix <- function(x = matrix()) {
  
  invmatx<- NULL
## set matx value
  setmatx<- function(y){
    x<<- y
    invmatx<<- NULL
  
}
  getmatx<- function() x
  setinv<- function(inv) invmatx<<- inv
  getinv<- function() invmatx
  list(setmatx = setmatx, getmatx = getmatx, setinv = setinv, getinv = getinv)


  }

## Write a short comment describing this function

## this function retrieves previous matrix as new input and tests the inverse value. 
## Matrix data retrieved and inversion set with solve(). 
##if Inv matrix has value, "retrieving cache" message will read with data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invmatx<- x$getinv()
  
  if(!is.null(invmatx)){
    message("retrieving cache")
    return(invmatx)
  }
  matx<- x$getmatx()
  invmatx<- solve(matx, ...)
  x$setinv(invmatx)
  return(invmatx)
}
