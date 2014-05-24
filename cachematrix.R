## Filename     : cachematrix.R
## Course       : R Programming
## Assignement  : Programming Assignment 2: Caching the Inverse of a Matrix
## Author       : Purbasha C  Gupta
## Created      : 5/22/2014
## Description  : This file contains two functions makeCacheMatrix and cacheSolve. makeCacheMatrix creates a special matrix object that caches it's inverse. cacheSolve returns the cached matrix object if it exists.  Otherwise it creates the inverse of the matrix and stores it in the special matrix object.  makeCacheMatrix uses Lexical scoping to implement the caching mechanism. 

##Function      : makeCacheMatrix
## Argument     : Empty matrix object
## Output       : List of functions for setting and getting the matrix object and it's inverse. 
## Description  : This function stores a matrix and caches it's inverse. 
makeCacheMatrix <- function(x = matrix()) {
    invx <- NULL
    set <- function(m1) {
        x <<- m1
        invx <<- NULL
    }
    get <- function() x
    setinv <- function(m1) invx <<- m1
    getinv <- function() invx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


##Function      : cacheSolve
## Argument     : Special Matrix Object created by makeCacheMatrix
## Output       : Inverse of the matrix stored in special matrix.  
## Description  : If the special matrix already has a value stored this cached value is returned. Otherwise the function creates the inverse using solve function and stores it in the special matrix and returns it as output. 
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    m <- x$get()
    invm <- solve(m)
    x$setinv(invm)
    invm
}

## Steps for basic verification
# > m1 <- matrix(c(3,-7,5,2),2,2)
# > m1
# [,1] [,2]
# [1,]    3    5
# [2,]   -7    2
# > m2 <- makeCacheMatrix(m1)
# > m2inv <- cacheSolve(m2)
# > m2inv
# [,1]        [,2]
# [1,] 0.04878049 -0.12195122
# [2,] 0.17073171  0.07317073
