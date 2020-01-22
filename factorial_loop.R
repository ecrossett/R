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
