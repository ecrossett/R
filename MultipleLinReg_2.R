# initialize data
x1 = c(100,100,110,110,120,120,120,130,130,130)
x2 = c(1,2,1,2,1,2,3,1,2,3)
x2sq = c(1,4,1,4,1,4,9,1,4,9)
x2sum = x2 + x2sq
y = c(113,118,127,132,136,144,138,146,156,149)
Y = as.matrix(y)
X = as.matrix(x1)
int = rep(1,10)
X = cbind(x1,x2,x2sq)


X[,2] %*% Y

sumy = 0
for(i in 1:10){
    sumy = sumy + y[i]
}
sumx1 = 0
for(i in 1:10){
    sumx1 = sumx1 + x1[i]
}

sumx2 = 0
for(i in 1:10){
    sumx2 = sumx2 + x2[i]
}

sumx1*sumy

betas <- solve(t(X) %*% X) %*% t(X) %*% Y
betas

X = cbind(int,x1,x2,x2sq)

# linear regression
lmX <- lm(Y ~ X, int = FALSE)
lmx2 <- lm(y ~ x2sum)
lmx12 <- lm(y ~ x1 + x2sum)
?lm
summary(lmx1)
summary(lmx2)
summary(lmx12)
lm(formula = y~x1 + x2)

plot(x1,y) 
abline(lmx12)

plot(x2sum,y)
abline(lmx2)

# nonlinear regression
b1_start <- 1
b2_start <- 1
b3_start <- 1
NLreg <- nls(y ~ b1*x1 + b2*x2 + b3*x2*x2)
summary(NLreg)

## plot
plot(x1,y)
lines(x1,predict(NLreg))


NLreg$fitted.values
plot(x2,y)
+lines(lmx1$fitted.values)
abline(lmfit)