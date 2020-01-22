library(purrr)
install.packages("microbenchmark")
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)
install.packages("benchmarkme")
library(benchmarkme)

test_values <- c(0,10,100,200,300,400,500,600,700,800,900,1000)

fact_data1 <- map(test_values, ~ microbenchmark(
                                    factorial_loop(.),
                                    factorial_func(.),
                                    factorial_reduce(.),
                                    factorial_mem(.)
                                    ))
names(fact_data1) <- as.character((test_values))
fact_data1

# Run benchmark
fact_loop_data <- map(test_values, function(n){microbenchmark(
                                    factorial_loop(n), times = 100)$time})
fact_func_data <- map(test_values, function(n){microbenchmark(
                                    factorial_func(n), times = 100)$time})
fact_reduce_data <- map(test_values, function(n){microbenchmark(
                                    factorial_reduce(n), times = 100)$time})
fact_mem_data <- map(test_values, function(n){microbenchmark(
                                    factorial_mem(n), times = 100)$time})
# Rename Test Value Headings                               
names(fact_loop_data) <- paste0(letters[1:length(test_values)],1:length(test_values))
names(fact_func_data) <- paste0(letters[1:length(test_values)],1:length(test_values))
names(fact_reduce_data) <- paste0(letters[1:length(test_values)],1:length(test_values))
names(fact_mem_data) <- paste0(letters[1:length(test_values)],1:length(test_values))

# Convert to df
fact_loop_data <- as.data.frame(fact_loop_data)
fact_func_data <- as.data.frame(fact_func_data)
fact_reduce_data <- as.data.frame(fact_reduce_data)
fact_mem_data <- as.data.frame(fact_mem_data)
# Summarize Results
fact_loop_data %<>%
    gather(num,time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))
fact_func_data %<>%
    gather(num,time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))
fact_reduce_data %<>%
    gather(num,time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))
fact_mem_data %<>%
    gather(num,time) %>%
    group_by(num) %>%
    summarise(med_time = median(time))

# Rename rows
fact_loop_data$num <- rownames(fact_loop_data)
fact_func_data$num <- rownames(fact_loop_data)
fact_reduce_data$num <- rownames(fact_loop_data)
fact_mem_data$num <- rownames(fact_loop_data)

fact_loop_data 
fact_func_data 
fact_reduce_data 
fact_mem_data 
# Rename num to n
names(fact_loop_data)[1] <- "n"
names(fact_func_data)[1] <- "n"
names(fact_reduce_data)[1] <- "n"
names(fact_mem_data)[1] <- "n"
# Create list
factorial_summary <- list(fact_loop_data,fact_func_data,fact_reduce_data, fact_mem_data)
names(factorial_summary) <- c("loop", "func", "reduce", "mem")
factorial_summary 
# Create data frame
factorial_df <- as.data.frame(factorial_summary)
factorial_df
# Plot results
plot(test_values, fact_reduce_data$med_time,
     xlab = "n value for n!",
     ylab = "Median Time (Nanoseconds)",
     pch = 18, bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = test_values)
axis(2, at = seq(0, 650000, by = 50000)) #1:max(fact_reduce_data$med_time))
lines(test_values, fact_func_data$med_time, col = "blue", pch = 18)
lines(test_values +.1, fact_loop_data$med_time, col = "magenta", pch = 18)
lines(test_values, fact_mem_data$med_time, col = "green", pch = 18)
lines(test_values, fact_reduce_data$med_time, col = "black", pch = 18)
legend("topleft", legend = c("reduce","func","loop","mem"), pch = 18,
       col = c("black","blue", "magenta", "green"), bty = "n", cex = 1, y.intersp = 1.5)
grid(nx = NULL, ny = nx,col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
# Run benchmark for ranges
range_table <- c(rep(NA,100))
benchmark_range <- function(n){
    microbenchmark(map(n, factorial_loop),
                   map(n, factorial_func),
                   map(n, factorial_reduce),
                   map_dbl(n, factorial_mem))
}
bench_range <-list(`range 1:10` = 1:10,
                   `range 10:20` = 10:20,
                   `range 20:30` = 20:30,
                   `range 30:40` = 30:40,
                   `range 40:50` = 40:50,
                   `range 50:60` = 50:60,
                   `range 60:70` = 60:70,
                   `range 70:80` = 70:80,
                   `range 80:90` = 80:90,
                   `range 90:100` = 90:100)
bench_range_results <- map(bench_range, benchmark_range)
bench_range_results
