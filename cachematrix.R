## These functions calculate the inverse of a matrix and cache the inverse.
## Once the matrix inverse is computed (by calling the "cacheSolve" function),
## the value of the inverse is cached until the values in the matrix are changed.

## makeCacheMatrix creates a special "matrix", which is really a list containing 
## the following functions:
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setmatinv(): set the value of the matrix inverse
## 4. getmatinv(): get the value of the matrix inverse.
## 
## While calling makeCacheMatrix, always make sure to assign it a variable, 
## and input a square matrix, e.g.,
## > myMat <- makeCacheMatrix(matrix(c(-1,3,2,0,5,6,0,-3,-2),ncol=3))
makeCacheMatrix <- function(x = matrix()) {
    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    setmatinv <- function(solve) mat_inv <<- solve
    getmatinv<- function() mat_inv
    list(set = set, get = get,
         setmatinv = setmatinv,
         getmatinv = getmatinv)
}


## This function calculates the inverse of matrix in x. If the matrix inverse
## of x already exists, the function returns the cached value.
## Always make sure to run this function using the variable to which the 
## makeCacheMatrix was assigned, e.g.,
## > myMat <- makeCacheMatrix(matrix(c(-1,3,2,0,5,6,0,-3,-2),ncol=3))
## > cacheSolve(myMat)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getmatinv()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    data <- x$get()
    mat_inv <- solve(data, ...)
    x$setmatinv(mat_inv)
    mat_inv
}
