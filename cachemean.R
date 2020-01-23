## Calculates the mean of the vector from the cache list created in 
## makeVector.R.  Checks to see if the mean has already been calculated.
## If, so it gets the mean from the cache and skips the computation.
## Otherwise, it caclulates the mean of the data and sets the value of
## the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
