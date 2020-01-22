## This function uses a recursion function to calculate n! 
factorial_func <- function(n) {
    if(!is.numeric(n) || n < 0 || n %% 1 != 0){    # Check inputs
        stop("must enter positive integer")        # Error if not a positive integer
    } else if (n == 0){
        return(1)                                  # Return 1 if n = 0
    }
    n * factorial_func(n - 1)                      # Recursive function to calc n!
}

