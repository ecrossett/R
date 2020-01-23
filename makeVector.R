## Create a special "vector" list which sets the value of a vector,
## gets the value of the vector, sets the value of the mean,
## and gets the value of the mean.  Works in conjunction with 
## cachemean.R function.

makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get, setmean = setmean, getmean = getmean)
}
