# This file contains two functions. The first function "makeCacheMatrix"
# takes a given matrix (defaulted to an empty matrix) and creates four functions
# as attributes of the matrix objects. The return is a list of the four funcs.
# The second function, is the "cacheSolve" function which determines whether 
# a cached value of an inverse to a matrix is found or not. If not, calculates the
# inverse and returns it.
# This code can be tested as follows (the assumption that the input matrix is invertible)
# t <- rbind(c(2,3,-1),-11:-9,8:10)
# x <- makeCacheMatrix(t)
# cacheSolve(x) # first time, it just calculates it !!!check if the random input is invertible
# cacheSolve(x) # second time, it caches it

#' This function takes either a matrix object x or creates an empty matrix
#' and creates four functions as attributes of x: set, get, setinv and getinv 
makeCacheMatrix <- function(x = matrix()) {  
  # set the initial value of the inverse matrix to null
  inv <- NULL
 
  # a function to set the matrix object
  set <- function(y){
    # write over the x value in the parent env.
    x <<- y
    # set the value of inv to null in the parent env.
    inv <<- NULL
  }
  
  # a function to get the matrix object
  get <- function() x
  
  # a function to set the inverse of the matrix
  setinv <- function(y){
    inv <<- y   
  }
  
  # a function to get the inverse of the matrix
  getinv <- function() inv
  
  return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


#' This function checks whether the inverse of an invertible 
#' matrix is already cached. If yes, it returns the cached matrix. 
#' Otherwise, it calculates the inverse of the matrix using the
#' "solve" function in R.
cacheSolve <- function(x, ...) {
  # first, check if the matrix's inverse is already cached
  inv <- x$getinv()
  
  if (!is.null(inv)){
    message("The inverse is already cached. Returning the cached matrix..")
    return(inv)
  }

  # if not cached, find the inverse matrix and set its value
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  # return the inverse matrix
  return(inv)  
}