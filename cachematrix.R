## Improve computational efficiency of programs that repeatedly
## need to find the inverse of the same matrix by storing the inverse in
## a cache and retrieving that value if it exists.

## This function creates a special "matrix" object from a user
## input matrix A.  Function creates a list of four elements
## in order to store the inverse of A in a cache to improve
## computational efficiency.  Works in conjunction with cacheSolve.R.
makeCacheMatrix <- function(Ax = matrix()) {
    
    Ainv <- NULL                  ## Set A inverse to NULL
    set <- function(A) {          ## Call function to initialize matrix Ax
        Ax <<- A                  ## Set Ax = A
        Ainv <<- NULL             ## Initialize A inversse to NULL
    }
    ## Store Ax and Ainv initialization in cache list
    get <- function() Ax
    setInv <- function(solve) Ainv <<- solve
    getInv <- function() Ainv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the matrix Ax which
## was stored by function makeCacheMatrix.R.  Checks to see
## if the inverse has already been computed, and that the 
## matrix has not changed.  If either are false, then it computes
## the inverse of Ax, otherwise it retrieves the inverse from the
## cache list.
cacheSolve <- function(Ax, ...) {

    Ainv <- Ax$getInv()                   ## Retrieve Ainv cache value
    if(!is.null(Ainv)) {                  ## Check if Ainv is null
        message("getting cached data")
        return(Ainv)                      ## If NULL, return cached value
    }
    data <- Ax$get()                      ## Retrieve matrix Ax
    Ainv <- solve(data, ...)              ## Invert matrix Ax
    Ax$setInv(Ainv)                       ## Store Ainv in cache and return value
    Ainv
}
