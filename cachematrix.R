## Following 2 functions have been created as part of Programming Assignment 2.
## The first function "makeCacheMatrix" creates a special "matrix" object,
## that can cache its inverse.
## The second function "cacheSolve" computes the inverse of the special matrix
## returned by the first function "makeCacheMatrix"

## The first function, "makeCacheMatrix" creates a special matrix, 
## which is really a list containing a function to do the following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  set<-function(y)
    {
      x <<- y
      m <<- NULL
    }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## The following function "cacheSolve" computes the inverse of the matrix 
## returned by the above function "makeCacheMatrix".

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  if(!is.null(m))
    {
      message("Fetching cached matrix")
      return(m)
    }
  
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
