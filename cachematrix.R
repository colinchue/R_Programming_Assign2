## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Sample run:
# > source("cachematrix.R")
# > m <- matrix(c(2,6,7,9), nrow = 2, ncol = 2)
# > make <- makeCacheMatrix(m)
# > cacheSolve(make)
# [,1]        [,2]
# [1,] -0.375  0.29166667
# [2,]  0.250 -0.08333333

makeCacheMatrix <- function(x = matrix()) { #cache the inverse of the matrix
  invmat <- NULL #cache the inverse of the matrix
  
  getmat <- function() x #get the value of the matrix
  
  setmat <- function(y){ #set the value of the matrix
    x <<- y
    invmat <- NULL
  }
  
  getinvmat <- function() invmat #get the inverse of the matrix
  setinvmat <- function(inverse) invmat <<- inverse #set the inverse of the matrix
  
  #generate a list of cache values of the matrix
  list(setmat = setmat, getmat = getmat, setinvmat = setinvmat, getinvmat = getinvmat)
  

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getinvmat() #get the value of the inverse of the matrix
  
  if(!is.null(invmat)){ #check if matrix has been cached and skip the statements below
    message("inverse is cached")
    return(invmat)
  }
  
  #get the matrix value and assign it to a variable
  datamat <- x$getmat() 
  
  #find the inverse of the matrix using the built-in R solve function
  invmat <- solve(datamat,...)
  
  #set the inverse of the matrix
  x$setinvmat(invmat)
  
  #return the inverse of the matrix value
  return(invmat)
    
}
