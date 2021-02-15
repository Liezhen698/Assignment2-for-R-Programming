## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, `makeCacheMatrix` creates a special "matrix", which is
##  a list containing a function to

## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse matrix
## 4.  get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inMatrix <- NULL      ## initiate an empty matrix to be the inverse matrix  
  set <- function(y) {
    x <<- y             ## set the value of the matrix 
    inMatrix <<- NULL   ## reset the inverse matrix to be empty
    
  }
  get <- function() x
  setSolve <- function(solve) inMatrix <<- solve
  getSolve <- function() inMatrix
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)  
}

## Write a short comment describing this function
## The second function works in pair with the above function to get the 
## inverse matrix of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse matrix has already been 
## calculated. If it's already claculated, it `get`s the inverse matrix, 
## i.e. 'inMatrix', from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value
## of the inverse matrix in the cache via the `setSolve`
## function and print the inverse matrix of the input matrix in the console. 
## Note: if an error message occur when executing the function 'solve()' at the 
## step 'inMatrix <- solve(data, ...)', define the tolerance "tol" to be lower 
## than the 'Machine$double.eps', i.e. 'tol = 1e-17', could help to solve the problem.

cacheSolve <- function(x, ...) {
  inMatrix <- x$getSolve()
  if(!is.null(inMatrix)) {
    message("getting cached data")
    return(inMatrix)
  }
  data <- x$get()
  inMatrix <- solve(data, ...)   ## Note the tolerance 'tol' could be alternatively set here.
                              
                      
  x$setSolve(inMatrix)
  inMatrix           ## Return a matrix that is the inverse of 'x'
}
