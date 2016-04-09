## Put comments here that give an overall description of what your
## functions do

## Finding inverse of matrix with functions to set up cache

makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv_mat <<- inverse
  getinverse <- function() inv_mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates inverse of matrix unless it has been calculated
## From makeCacheMatrix
## If already calculated, gets from cache and skips

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$getinverse()
  if(!is.null(inv_mat)) {
    print("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$setinverse(inv_mat)
  inv_mat
}

## TEST
# test <- matrix(1:10,2)
# class(test)
# test_example <- makeCacheMatrix(test)
# test_example$get()
# cacheSolve(test_example)
## ISSUE WITH TEST ABOVE IS THAT NON SQUARE MATRIX

##TEST 2
test <- matrix(c(3,1,4,2),2)
class(test)
test_example <- makeCacheMatrix(test)
test_example$get()
cacheSolve(test_example)
cacheSolve(test_example)
test_example$getinverse()