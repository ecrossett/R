getwd()
setwd("/Users/ed/Documents/R/Statistics")
library(ggplot2)
library(MASS)

# Initialize variables
T = 1000
n = 100
x = (1:100)
set.seed(1234)
phi = matrix(c(0.75,0.6,0.25,0.4),2,2)
prob1 = rep(.75,100)
prob2 = rep(.6,100)
s = rep(0,100)

s[1] = sample(c(1,2),size = 1,replace = TRUE,prob = c(0.5,0.5))
# Loop through markov chain
for(i in 1:n){
    if(s[i] == 1){
        s[i+1] = sample(c(1,2),size = 1,replace = TRUE,prob = phi[1,])
        prob1[i] = phi[1,1]
        prob2[i] = phi[1,2]
    } else if(s[i] == 2){
        s[i+1] = sample(c(1,2),size = 1,replace = TRUE,prob = phi[2,])
        prob1[i] = phi[2,1]
        prob2[i] = phi[2,2]
    }
}

prob <- cbind.data.frame(prob1,prob2,s)
# Plot results
plot(s)
hist(s)
hist(prob1, ylim =c(0,80))
hist(prob2,ylim =c(0,80))
for(i in 1:n+1){
    p = phi^i
} 

# Find Stationary Distribution

one = c(1,1)
G = phi - matrix(c(1,0,1,0),2,2)
G[,2] = one
Ginv = ginv(G)
stationary = c(0,0)
stationary[1] = 0*Ginv[1,1] + 1*Ginv[2,1]
stationary[2] = 0*Ginv[2,1] + 1*Ginv[2,2]
stationary


