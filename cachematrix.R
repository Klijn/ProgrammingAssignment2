## Create a "matrix" that is able to cache an inverse of itself

## Create a special "matrix", which is a list containing functions to get/set
## the matrix, and to get/set the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y                                 ## new values for matrix
                inv <<- NULL                            ## reset inverse matrix
        }
        get <- function() x                             ## return matrix
        setsolve <- function(solve) inv <<- solve       ## calculate inverse
        getsolve <- function() inv                      ## return inverse matrix
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

## Return inverse matrix of the custom "matrix" object x
cacheSolve <- function(x, ...) {
        inv <- x$getsolve()                             ## check cache
        if(!is.null(inv)){
                message("retrieved cached data")
                return(inv)                             ## return cached inverse
        }
        data <- x$get()                                 ## get matrix
        inv <- solve(data, ...)                         ## calculate inverse
        x$setsolve(inv)                                 ## cache inverse
        inv                                             ## return inverse
}


## TESTING 2x2 MATRIX
## correct inverse matrix is:
##  0.6   -0.7
## -0.2   0.4
matrix2 <- makeCacheMatrix(matrix(c(4,2,7,6), nrow=2))
cacheSolve(matrix2)
cacheSolve(matrix2)

## TESTING 3x3 MATRIX
## correct inverse matrix is:
##  0.2   0.2    0
## -0.2   0.3    1
##  0.2  -0.3    0
matrix3 <- makeCacheMatrix(matrix(c(3,2,0,0,0,1,2,-2,1), nrow=3))
cacheSolve(matrix3)
cacheSolve(matrix3)