## This function calulates n! using an iterative for loop
factorial_loop <- function(n){
    if(!is.numeric(n) || n < 0 || n %% 1 != 0){     # Check inputs
        stop("must enter positive integer")         # Error if not a positive integer
    } else if (n == 0){
        return(1)                                   # Return 1 if n = 0
    }
    k <- 1                                          # Initialize k
    for(i in 1:n){                                  # Iterate from 1 to n
        k <- k*i                                    # Compute factorial and store in k
    }
    k                                               # Return final value k
}


library(purrr)
## This function uses purrr library and reduce function to compute n!
factorial_reduce <- function(n){
    if(!is.numeric(n) || n < 0 || n %% 1 != 0){      # Check inputs
        stop("must enter positive integer")          # Error if not a positive integer
    } else if (n == 0){
        return(1)                                    # Return 1 if n = 0
    }
    reduce(1:n, `*`)                                 # Compute n! for 1 to n
}


## This function uses a recursion function to calculate n! 
factorial_func <- function(n) {
    if(!is.numeric(n) || n < 0 || n %% 1 != 0){    # Check inputs
        stop("must enter positive integer")        # Error if not a positive integer
    } else if (n == 0){
        return(1)                                  # Return 1 if n = 0
    }
    n * factorial_func(n - 1)                      # Recursive function to calc n!
}

fact_tbl <- c(rep(NA,1000))
## This function creates a memory table to store each value of each
## iteration while caclulating n! recursively.  The idea is the same
## as the factorial_func function but strives to optimize computational
## efficiency. 
factorial_mem <- function(n){
    if(!is.numeric(n) || n < 0 || n %% 1 != 0){     # Check inputs
        stop("must enter positive integer")         # Error if not a positive integer
    } else if (n == 0){
        return(1)                                   # Return 1 if n = 0
    }
    if(!is.na(fact_tbl[n])){            # Check if n-th element of memory table exists
        fact_tbl[n]                     # If so, then we are done and returns result
    } else{
        fact_tbl[n] <<- n * factorial_mem(n - 1)    # Populate using last avail value
        fact_tbl[n]                                 # Return final value of n!
    }
}
