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



