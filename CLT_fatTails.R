<font size="5">  M i.i.d. draws of length N from the tν distribution. </font> 

```{r messages = FALSE, warning = FALSE}
library(dplyr)
library(statsr)
library(ggplot2)
library(devtools)
M = 500
N = 10000
v = 100
set.seed(1234)
samples = rep(0,M)

for(i in 1:M){
    samples[i] <- rt(N,df = v)
}
hist(samples, main = "Distribution of 500 Samples from tv Distribution", xlab = "Sample Values")

```

<font size="5"> For each sample m = 1,...,M, compute the sample mean for the first n = 5,10,100,500,1,000,10,000 draws of the sample.</font> 


```{r messages = FALSE, warning = FALSE}
set.seed(1234)
M = 500
N = 10000
v = 100

s5 <- rep(0,M)
s10 <-rep(0,M)
s100 <-rep(0,M)
s500 <- rep(0,M)
s1000 <-rep(0,M)
s10000 <-rep(0,M)

set.seed(1234)
for (i in 1:M){
        s5[i] <- mean(rt(5,df = v)) 
        #sd5[i] <- sd(rt(5,df = v))
        #var5[i] <- var(rt(5,df = v))
        
}

set.seed(1234)
for (i in 1:M){
        s10[i] <- mean(rt(10,df = v))
         #sd10[i] <- sd(rt(10,df = v))
        #var10[i] <- var(rt(10,df = v))
}

set.seed(1234)
for (i in 1:M){
        s100[i] <- mean(rt(100,df = v))
         #sd100[i] <- sd(rt(100,df = v))
        #var100[i] <- var(rt(100,df = v))
}

set.seed(1234)
for (i in 1:M){
        s500[i] <- mean(rt(500,df = v))
        # sd500[i] <- sd(rt(500,df = v))
       # var500[i] <- var(rt(500,df = v))
}

set.seed(1234)
for (i in 1:M){
        s1000[i] <- mean(rt(1000,df = v))
         #sd1000[i] <- sd(rt(1000,df = v))
        #var1000[i] <- var(rt(1000,df = v))
}

set.seed(1234)
for (i in 1:M){
        s10000[i] <- mean(rt(10000,df = v))
        #sd10000[i] <- sd(rt(10000,df = v))
        #var10000[i] <- var(rt(10000,df = v))
}


```

<font size="5"> For each n, compute the mean, standard deviation and variance of the sample mean across the M samples. </font> 



```{r messages = FALSE, warning = FALSE}
sm5m <- round(mean(s5),4)
sd5 <- sd(s5)
var5 <- var(s5)

sm10m <- round(mean(s10),4)
sd10 <- sd(s10)
var10 <- var(s10)

sm100m <- round(mean(s100),4)
sd100 <- sd(s100)
var100 <- var(s100)

sm500m <- round(mean(s500),4)
sd500 <- sd(s500)
var500 <- var(s500)

sm1000m <- round(mean(s1000),4)
sd1000 <- sd(s1000)
var1000 <- var(s1000)

sm10000m <- round(mean(s10000),4)
sd10000 <- sd(s10000)
var10000 <- var(s10000)

means <- c(sm5m,sm10m,sm100m,sm500m,sm1000m,sm10000m)
sds <- c(sd5,sd10,sd100,sd500,sd1000,sd10000)
vars <- c(var5,var10,var100,var500,var1000,var10000)


statsTable <- data.frame(means,sds,vars)
colnames(statsTable) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable <- as.matrix(statsTable)
statsTable

```


<font size = "5"> plot the distributions of the M sample means.</font>

```{r}
hist(s5, main = "m = 500, n = 5", xlab = "Sample Means")
hist(s10, main = "m = 500, n = 10", xlab = "Sample Means")
hist(s100, main = "m = 500, n = 100", xlab = "Sample Means")
hist(s1000, main = "m = 500, n = 1000", xlab = "Sample Means")
hist(s10000, main = "m = 500, n = 10000", xlab = "Sample Means")
```


```{r messages = FALSE, warning = FALSE}
set.seed(1234)
M = 500
N = 10000
v = 100
vs = c(10, 5, 2, 1, 0.5)

sm5.10 <- rep(0,M)
sm5.5 <- rep(0,M)
sm5.2 <- rep(0,M)
sm5.1 <- rep(0,M)
sm5.0.5 <- rep(0,M)

sm10.10 <- rep(0,M)
sm10.5 <- rep(0,M)
sm10.2 <- rep(0,M)
sm10.1 <- rep(0,M)
sm10.0.5 <- rep(0,M)

sm100.10 <- rep(0,M)
sm100.5 <- rep(0,M)
sm100.2 <- rep(0,M)
sm100.1 <- rep(0,M)
sm100.0.5 <- rep(0,M)

sm500.10 <- rep(0,M)
sm500.5 <- rep(0,M)
sm500.2 <- rep(0,M)
sm500.1 <- rep(0,M)
sm500.0.5 <- rep(0,M)

sm1000.10 <- rep(0,M)
sm1000.5 <- rep(0,M)
sm1000.2 <- rep(0,M)
sm1000.1 <- rep(0,M)
sm1000.0.5 <- rep(0,M)

sm10000.10 <- rep(0,M)
sm10000.5 <- rep(0,M)
sm10000.2 <- rep(0,M)
sm10000.1 <- rep(0,M)
sm10000.0.5 <- rep(0,M)

set.seed(1234)
for (i in 1:M){
        sm5.10[i] <- mean(rt(5,df = 10))
        sm5.5[i] <- mean(rt(5,df = 5))
        sm5.2[i] <- mean(rt(5,df = 2))
        sm5.1[i] <- mean(rt(5,df = 1))
        sm5.0.5[i] <- mean(rt(5,df = 0.5))
        
}


set.seed(1234)
for (i in 1:M){
        sm10.10[i] <- mean(rt(10,df = 10))
        sm10.5[i] <- mean(rt(10,df = 5))
        sm10.2[i] <- mean(rt(10,df = 2))
        sm10.1[i] <- mean(rt(10,df = 1))
        sm10.0.5[i] <- mean(rt(10,df = 0.5))
        
}

set.seed(1234)
for (i in 1:M){
        sm100.10[i] <- mean(rt(100,df = 10))
        sm100.5[i] <- mean(rt(100,df = 5))
        sm100.2[i] <- mean(rt(100,df = 2))
        sm100.1[i] <- mean(rt(100,df = 1))
        sm100.0.5[i] <- mean(rt(100,df = 0.5))
        
}

set.seed(1234)
for (i in 1:M){
        sm500.10[i] <- mean(rt(500,df = 10))
        sm500.5[i] <- mean(rt(500,df = 5))
        sm500.2[i] <- mean(rt(500,df = 2))
        sm500.1[i] <- mean(rt(500,df = 1))
        sm500.0.5[i] <- mean(rt(500,df = 0.5))
        
}

set.seed(1234)
for (i in 1:M){
        sm1000.10[i] <- mean(rt(1000,df = 10))
        sm1000.5[i] <- mean(rt(1000,df = 5))
        sm1000.2[i] <- mean(rt(1000,df = 2))
        sm1000.1[i] <- mean(rt(1000,df = 1))
        sm1000.0.5[i] <- mean(rt(1000,df = 0.5))
        
}

set.seed(1234)
for (i in 1:M){
        sm10000.10[i] <- mean(rt(10000,df = 10))
        sm10000.5[i] <- mean(rt(10000,df = 5))
        sm10000.2[i] <- mean(rt(10000,df = 2))
        sm10000.1[i] <- mean(rt(10000,df = 1))
        sm10000.0.5[i] <- mean(rt(10000,df = 0.5))
        
}


# 5
sm5m <- list(s5,sm5.10,sm5.5,sm5.2,sm5.1,sm5.0.5)
sm5m <- sapply(sm5m,function(x) round(mean(x),4))

sd5 <- list(s5,sm5.10,sm5.5,sm5.2,sm5.1,sm5.0.5)
sd5sd <- sapply(sd5,function(x) sd(x))

var5 <- list(s5,sm5.10,sm5.5,sm5.2,sm5.1,sm5.0.5)
var5v <- sapply(var5,function(x) round(var(x),4))

# 10
sm10m <- list(s10,sm10.10,sm10.5,sm10.2,sm10.1,sm10.0.5)
sm10m <- sapply(sm10m,function(x) round(mean(x),4))

sd10 <- list(s10,sm10.10,sm10.5,sm10.2,sm10.1,sm10.0.5)
sd10sd <- sapply(sd10,function(x) round(sd(x),4))

var10 <- list(s10,sm10.10,sm10.5,sm10.2,sm10.1,sm10.0.5)
var10v <- sapply(var1000,function(x) round(var(x),4))

# 100
sm100m <- list(s100,sm100.10,sm100.5,sm100.2,sm100.1,sm100.0.5)
sm100m <- sapply(sm100m,function(x) round(mean(x),4))

sd100 <- list(s100,sm100.10,sm100.5,sm100.2,sm100.1,sm100.0.5)
sd100sd <- sapply(sd100,function(x) round(sd(x),4))

var1000 <- list(s100,sm100.10,sm100.5,sm100.2,sm100.1,sm100.0.5)
var1000v <- sapply(var100,function(x) round(var(x),4))

# 500
sm500m <- list(s500,sm500.10,sm500.5,sm500.2,sm500.1,sm500.0.5)
sm500m <- sapply(sm500m,function(x) round(mean(x),4))

sd500 <- list(s500,sm500.10,sm500.5,sm500.2,sm500.1,sm500.0.5)
sd500sd <- sapply(sd500,function(x) round(sd(x),4))

var500 <- list(s500,sm500.10,sm500.5,sm500.2,sm500.1,sm500.0.5)
var500v <- sapply(var500,function(x) round(var(x),4))

# 1,000
sm1000m <- list(s1000,sm1000.10,sm1000.5,sm1000.2,sm1000.1,sm1000.0.5)
sm1000m <- sapply(sm1000m,function(x) round(mean(x),4))

sd1000 <- list(s1000,sm1000.10,sm1000.5,sm1000.2,sm1000.1,sm1000.0.5)
sd1000sd <- sapply(sd1000,function(x) round(sd(x),4))

var1000 <- list(s1000,sm1000.10,sm1000.5,sm1000.2,sm1000.1,sm1000.0.5)
var1000v <- sapply(var1000,function(x) round(var(x),4))

# 10,000
sm10000m <- list(s10000,sm10000.10,sm10000.5,sm10000.2,sm10000.1,sm10000.0.5)
sm10000m <- sapply(sm10000m,function(x) round(mean(x),4))

sd10000 <- list(s10000,sm10000.10,sm10000.5,sm10000.2,sm10000.1,sm10000.0.5)
sd10000sd <- sapply(sd10000,function(x) round(sd(x),4))

var10000 <- list(s10000,sm10000.10,sm10000.5,sm10000.2,sm10000.1,sm10000.0.5)
var10000v <- sapply(var10000,function(x) round(var(x),4))


#v = 10
means.10 <- list(sm5.10,sm10.10,sm100.10,sm500.10,sm1000.10,sm10000.10)
means.10m <- sapply(means.10, function(x) round(mean(x),4))

sd.10 <- list(sm5.10,sm10.10,sm100.10,sm500.10,sm1000.10,sm10000.10)
sd.10sd <- sapply(sd.10, function(x) round(sd(x),4))

var.10 <- list(sm5.10,sm10.10,sm100.10,sm500.10,sm1000.10,sm10000.10)
var.10v <- sapply(var.10, function(x) round(var(x),4))

statsTable10 <- data.frame(means.10m,sd.10sd,var.10v)
colnames(statsTable10) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable10) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable10 <- as.matrix(statsTable10)
statsTable10

#v = 5
means.5 <- list(sm5.5,sm10.5,sm100.5,sm500.5,sm1000.5,sm10000.5)
means.5m <- sapply(means.5, function(x) round(mean(x),4))

sd.5 <- list(sm5.5,sm10.5,sm100.5,sm500.5,sm1000.5,sm10000.5)
sd.5sd <- sapply(sd.5, function(x) round(sd(x),4))

var.5 <- list(sm5.5,sm10.5,sm100.5,sm500.5,sm1000.5,sm10000.5)
var.5v <- sapply(var.5, function(x) round(var(x),4))

statsTable5 <- data.frame(means.5m,sd.5sd,var.5v)
colnames(statsTable5) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable5) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable5 <- as.matrix(statsTable5)
statsTable5

#v = 2
means.2 <- list(sm5.2,sm10.2,sm100.2,sm500.2,sm1000.2,sm10000.2)
means.2m <- sapply(means.2, function(x) round(mean(x),4))

sd.2 <- list(sm5.2,sm10.2,sm100.2,sm500.2,sm1000.2,sm10000.2)
sd.2sd <- sapply(sd.2, function(x) round(sd(x),4))

var.2 <- list(sm5.2,sm10.2,sm100.2,sm500.2,sm1000.2,sm10000.2)
var.2v <- sapply(var.2, function(x) round(var(x),4))

statsTable2 <- data.frame(means.2m,sd.2sd,var.2v)
colnames(statsTable2) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable2) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable2 <- as.matrix(statsTable2)
statsTable2
#v = 1
means.1 <- list(sm5.1,sm10.1,sm100.1,sm500.1,sm1000.1,sm10000.1)
means.1m <- sapply(means.1, function(x) round(mean(x),4))

sd.1 <- list(sm5.1,sm10.1,sm100.1,sm500.1,sm1000.1,sm10000.1)
sd.1sd <- sapply(sd.1, function(x) round(sd(x),4))

var.1 <- list(sm5.1,sm10.1,sm100.1,sm500.1,sm1000.1,sm10000.1)
var.1v <- sapply(var.1, function(x) round(var(x),4))

statsTable1 <- data.frame(means.1m,sd.1sd,var.1v)
colnames(statsTable1) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable1) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable1 <- as.matrix(statsTable1)
statsTable1
#v = 0.5
means.0.5 <- list(sm5.0.5,sm10.0.5,sm100.0.5,sm500.0.5,sm1000.0.5,sm10000.0.5)
means.0.5m <- sapply(means.0.5, function(x) round(mean(x),4))

sd.0.5 <- list(sm5.0.5,sm10.0.5,sm100.0.5,sm500.0.5,sm1000.0.5,sm10000.0.5)
sd.0.5sd <- sapply(sd.0.5, function(x) round(sd(x),4))

var.0.5 <- list(sm5.0.5,sm10.0.5,sm100.0.5,sm500.0.5,sm1000.0.5,sm10000.0.5)
var.0.5v <- sapply(var.0.5, function(x) round(var(x),4))

statsTable0.5 <- data.frame(means.0.5m,sd.0.5sd,var.0.5v)
colnames(statsTable0.5) <- c("Sample Mean","Standard Deviation", "Variance")
rownames(statsTable0.5) <- c("n=5", "n=10","n=100","n=500", "n=1000","n=10000")
statsTable0.5 <- as.matrix(statsTable0.5)
statsTable0.5


```

```{r}
# v = 10
hist(sm5.10, main = "v = 10, m = 500, n = 5", xlab = "Sample Means")
hist(sm10.10, main = "v = 10, m = 500, n = 10", xlab = "Sample Means")
hist(sm100.10, main = "v = 10, m = 500, n = 100", xlab = "Sample Means")
hist(sm1000.10, main = "v = 10, m = 500, n = 1000", xlab = "Sample Means")
hist(sm10000.10, main = "v = 10, m = 500, n = 10000", xlab = "Sample Means")

# v = 5
hist(sm5.5, main = "v = 5, m = 500, n = 5", xlab = "Sample Means")
hist(sm10.5, main = "v = 5, m = 500, n = 10", xlab = "Sample Means")
hist(sm100.5, main = "v = 5, m = 500, n = 100", xlab = "Sample Means")
hist(sm1000.5, main = "v = 5, m = 500, n = 1000", xlab = "Sample Means")
hist(sm10000.5, main = "v = 5, m = 500, n = 10000", xlab = "Sample Means")

# v = 2
hist(sm5.2, main = "v = 2, m = 500, n = 5", xlab = "Sample Means")
hist(sm10.2, main = "v = 2, m = 500, n = 10", xlab = "Sample Means")
hist(sm100.2, main = "v = 2, m = 500, n = 100", xlab = "Sample Means")
hist(sm1000.2, main = "v = 2, m = 500, n = 1000", xlab = "Sample Means")
hist(sm10000.2, main = "v = 2, m = 500, n = 10000", xlab = "Sample Means")

# v = 1
hist(sm5.1, main = "v = 1, m = 500, n = 5", xlab = "Sample Means")
hist(sm10.1, main = "v = 1, m = 500, n = 10", xlab = "Sample Means")
hist(sm100.1, main = "v = 1, m = 500, n = 100", xlab = "Sample Means")
hist(sm1000.1, main = "v = 1, m = 500, n = 1000", xlab = "Sample Means")
hist(sm10000.1, main = "v = 1, m = 500, n = 10000", xlab = "Sample Means")

# v = 0.5
hist(sm5.0.5, main = "v = 0.5, m = 500, n = 5", xlab = "Sample Means")
hist(sm10.0.5, main = "v = 0.5, m = 500, n = 10", xlab = "Sample Means")
hist(sm100.0.5, main = "v = 0.5, m = 500, n = 100", xlab = "Sample Means")
hist(sm1000.0.5, main = "v = 0.5, m = 500, n = 1000", xlab = "Sample Means")
hist(sm10000.0.5, main = "v = 0.5, m = 500, n = 10000", xlab = "Sample Means")


```




