## makeCacheMatrix function returns a list containing get and set functions for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

  invmtrx <- NULL
  
  #Set function for the matrix
  set <- function(pmtrx1)
  {
    x <<- pmtrx1
    return (invmtrx <<- NULL)
  }

  # get function for the matrix  
  get <- function() return (x)

  #set function for the inverse  
  setinv <- function(pinvmtrx) return (invmtrx <<- pinvmtrx)

  #get function for the inverse  
  getinv <- function() return (invmtrx)

  # return 
  return (list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## cacheSolve function returns inverse of matrix. First it checks if the inverse is already cached, if not then it calculate the inverse
## for the matrix and set it in the cache for future calls before returning.

cacheSolve <- function(x, ...) {
  invmtrx <- x$getinv()
  
  # if the inverse matrix is already calculated. print the message  
  if (!is.null(invmtrx))
    print("...cached...")
  else # calculate inverse and cache it
  {
    mtrxdata <- x$get()
    invmtrx <- solve(mtrxdata, ...)
    x$setinv(invmtrx)
  }

  # return the inverse matrix
  return(invmtrx)
}

