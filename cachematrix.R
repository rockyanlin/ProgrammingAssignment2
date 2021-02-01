## Studied the sample code of Caching the Mean of a Vector, use that code to transform
## the functions to do the matrix inverse, note: matrix vs vector, and inverse vs mean.

## The <<- operator is used to assign a value to an object in an environment 
## that is different from the current environment.

## The first function, makeCacheMatrix creates a special “matrix”,  
## is a list containing a function to do the following:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## The second function cacheSolve calculates the inverse of matrix based on 
## the returned object from the first function (makeCacheMatrix) method
## getinverse, 1). when the object value is NULL, it calculates the inverse of 
## matrix and sets the inverse of matrix in the cache via the setinverse method
## and return it; 2). when the object value is not NULL - an existing cached 
## inverse matrix, then return it.  
## As instruction I use solve method for the inverse matrix, other methods like
## inverse(), or Inverse() can achieve similar results.

cacheSolve <- function(x, ...) {
  ## attempt to get the inverse of the matrix stored in cache
  inv <- x$getinverse()
  # return inverted matrix from cache if it exists
  # else create the matrix in working environment
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()

  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## The below is the part to test/check my created two functions work as designed 
## (1). set and cache those calculated inverse matrix
matrix_1 <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(matrix_1)

a1 <- c(3, 2, 8)  
a2 <- c(6, 3, 2)  
a3 <- c(5, 2, 4) 

A <- makeCacheMatrix(rbind(a1, a2, a3))
cacheSolve(A)

B <- makeCacheMatrix(matrix(c(2, 5, 3, 4, 5, 2, 6, 3, 4), 3, 3))
cacheSolve(B)

C <- matrix(c(1,8,4,2,4,5,5,1,2,2,5,4,4,1,1,2),nrow=4)
D <- makeCacheMatrix(C)
cacheSolve(D)

## (2). get and find those cached inverse matrix 
cacheSolve(matrix_1)
cacheSolve(D)
cacheSolve(A)
cacheSolve(B)

## (3). Is another matrix or 'new' is cached?
D1 <- matrix(c(1,2,3,4),2,2)
D2 <- makeCacheMatrix(D1)
cacheSolve(D1)
cacheSolve(D2)
