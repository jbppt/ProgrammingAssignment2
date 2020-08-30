## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function creates a matrix object that can
# cache it's inverse. In simple terms, we can pass a matrix through
# the function and receive four components that "process" the matrix.
# Similar to the analogous 'makeVector' example in the assignment
# description, the four directions - which can be called upon - set the
# value of the matrix, get the value of the matrix, set the value of the
# inverse, and get the value of the inverse. In this function we can
# modify the value of a matrix in question. This can happen because
# we used a double arrow assignment operator to assign the matrix 'y'
# (within the child environment 'set') to the matrix 'x' in it's parent
# environment 'makeCacheMatrix'. The double arrow assignment operator
# allows us to assign object names in a child environment to matching
# object names in a parent environment.


makeCacheMatrix <- function(x = matrix()) {
  matINV <- NULL
  set <- function(y){
    x <<- y
    matINV <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {matINV <<- inverse}
  getInverse <- function() {matINV}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

# Similar to the analagous 'cachemean' function in the assignment
# description, the 'cacheSolve' function below calculates the inverse of
# a matrix argument passed into 'makeCacheMatrix'. The first couple of
# lines in cacheSolve checks the cache space of makeCacheMatrix to see if
# an inverse has already been calculated. If so it will retrieve the
# inverse - otherwise, it will calculate the inverse of the matrix and
# then set the value of the inverse within makeCacheMatrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matINV <- x$getInverse()
  if(!is.null(matINV)){
    message("getting cached data")
    return(matINV)
  }
  mat <- x$get()
  matINV <-solve(mat,...)
  x$setInverse(matINV)
  matINV
}
