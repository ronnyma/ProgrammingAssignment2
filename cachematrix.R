## makeCacheMatrix: a more elaborated matrix object, which supports caching of
##                  its inverse matrix.
##
## cacheSolve:      similar to the R-function solve(M), but will operate in
##                  conjunction with makeCacheMatrix and utilize cached results
##                  to save time when available. 


## makeCacheMatrix():
## Takes a matrix M as argument and returns a list of functions
## operating on the matrix: nameCacheMatrix(M)
##
## +setMat(M): set the matrix, if needed. Will also clear the cached inverse.
## +getMat(): return the matrix
##
##  The functions below is usually only called by cacheSolve()
## -getInv(): get the inverse
## -setInv(solve(M)): set the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    setMat <- function(y) {
        x <<- y
        inv <<- NULL
    }

    getMat <- function() return(x)

    setInv <- function(i = matrix(i)) {
        inv <<- i 
    }

    getInv <- function() return(inv)

    # Now, build the list-object to be returned
    invisible(list(setMat = setMat, getMat = getMat,
         setInv = setInv, getInv = getInv))
}


## cacheSolve():
## Takes a makeCacheMatrix-list L and returns the inverse
## of L's contained matrix. This may or may not be cached.
## If no inverse is cached, a calculation is performed. 

cacheSolve <- function(x, ...) {

    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting cached data..")
        return(inv)
    }

    message("Cache is empty. Recalculating..")
    mat <- x$getMat()
    inv <- solve(mat)

    x$setInv(inv)

    # The last statement is returned implicitly
    inv
}
