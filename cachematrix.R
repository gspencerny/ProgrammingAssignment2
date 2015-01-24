## The makeCacheMatrix and cacheSolve functions work together
## to set and retrieve the target matrix value, calculate the inverse
## of the target matrix, store it, and retrieve it without recalculating.

## makeCacheMatrix creates a special object (a list) that
## essentially stores and retrieves values. It accepts a matrix,
## as well as four 'methods':
##   - set: accepts and stores a matrix
##   - get: returns the matrix stored with 'set'
##   - setinv: accepts and stores the inverse of the matrix stored in 'set'
##   - getinv: returns the inverse matrix stored with 'setinv'
## EXAMPLE USAGE:
##   this_matrix <- matrix(rnorm(9)), ncol=3, nrow=3)
##   z <- makeCacheMatrix(this_matrix)
##   z$get() # returns this_matrix

makeCacheMatrix <- function(x = matrix(), ...) {
  invmx <- NULL # initialize invmx, which will hold the inverse of the matrix
  set <- function(data) { 
    x <<- data # set the matrix variable, x (parent env)
    invmx <<- NULL # reset invmx (parent env) because the matrix changed
  }
  get <- function() x # return the matrix value
  setinv <- function(result) invmx <<- result # set the inverse matrix result (parent env)
  getinv <- function() invmx # return the inverse matrix
  list(set=set, get=get, # set attribute (method) order/names for clarity
       setinv=setinv, getinv=getinv) 
}

## cacheSolve returns a matrix that is the inverse of 'my_matrix'
##
##   cacheSolve works with makeCacheMatrix to solve the
##   inverse of 'my_matrix' and store the result in
##   makeCacheMatrix. If the result is already set, and the
##   matrix hasn't changed, this function skips calculating
##   the inverse and retrieves the stored/cached version.
## EXAMPLE USAGE:
##   this_matrix <- matrix(rnorm(9)), ncol=3, nrow=3)
##   z <- makeCacheMatrix(this_matrix)
##   cacheSolve(z)

cacheSolve <- function(my_matrix, ...) {
  thisinv <- my_matrix$getinv() # use $getinv to load any previously stored result
  # do we have a result, or NULL?
  if(!is.null(thisinv)) { # we have the result, grab it and return it
    message("loading cached data.")
    return(thisinv)
  }
  # we don't already have the result
  data <- my_matrix$get() # grab the target matrix using $get
  thisinv <- solve(data) # solve it
  my_matrix$setinv(thisinv) # store it using $setinv
  thisinv # return the result
}
