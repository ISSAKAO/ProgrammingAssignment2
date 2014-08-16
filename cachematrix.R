################################################################################################
#
#                               makeCacheMatrix function
#################################                       #########################################
#
# 
# makeCacheMatrix allow to caches the inverse of a matrix rather than compute it repeatedly. The
# 
#
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#################################################################################################
#
#                                      cacheSolve function  
###################################                         #####################################
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## the cachesolve should retrieve the inverse from the cache.
## cachseSolve retruns an inverted matrix using the built-in R function solve()
#################################################################################################


cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
                return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

