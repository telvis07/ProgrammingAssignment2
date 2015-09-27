## R Programming - Programming Assigment #2
## Telvis Calhoun
## 
## To test
## > ma = makeCacheMatrix(x=matrix(c(1,2,3,4), nrow=2, ncol=2))
## > cacheSolve(ma)
## > cacheSolve(ma) # shows message 'getting cached inverse matrix'

## makeCacheMatrix
## creates an CacheMatrix used to store a matrix and its solution.
## Arguments
##  x : A square numeric matrix
## Returns
##  a list containing functions to
##    set the value of the matrix (set)
##    get the value of the matrix (get)
##    set the value of the inverse matrix (setinv)
##    get the value of the inverse matrix (getinv)
makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinv <- function(inv_) inv_x <<- inv_
  getinv <- function() inv_x
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## This function 'solves' a matrix using the solve function
## then caches the solution in the 'CacheMatrix'. The function will
## read the cached solution after the first call using the same 'CacheMatrix' object.
## Arguments
## x : CacheMatrix object
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getinv()
  if(!is.null(inv_matrix)){
    message("getting cached inverse matrix")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data)
  x$setinv(inv_matrix)
  inv_matrix
}
