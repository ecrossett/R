getwd()
setwd("/Users/ed/Documents/R/Statistics")
library(ggplot2)



x <- read.csv("X.csv", header = FALSE)
y <- read.csv("Y.csv", header = FALSE)
head(x)
head(y)

int <- rep(1,1110)
Y <- as.matrix(y)
Y <- Y[,2:3]
X <- as.matrix(x)

X <- X[2:4,]
X <- X[,1:1110]

X <- t(X)
X <- cbind(int,X)

#X <- -X[-1,]
#X <- -X[-1111,]

# closed-form solution to manually calculate betas (B = (X'X)^-1 * X' * Y)
betas1 <- solve(t(X) %*% X) %*% t(X) %*% Y[,1]
betas2 <- solve(t(X) %*% X) %*% t(X) %*% Y[,2]
betas1
betas2
data <- cbind(X,Y)
data <- as.data.frame(data)

colnames(data) <- c("coef","X1", "X2","X3","Y1","Y2")
head(data)

# Compute P = (X*(X'X)^-1 * X')

P <- X %*% solve(t(X) %*% X) %*% t(X)

# Compute M = I - P
M <- matrix(0, 1110, 1110)
diag(M) <- 1
M[2,2]

# Check that MP = 0
check <- M %*% P
check[1]

# run OLS regression
lm.mod <- lm(Y ~ X)
lm.betas <- lm.mod$coefficients

# ======= Y1 ~ X1 ==================================
lm.y1 <- lm(Y[,1] ~ X[,2:4])
betay1 <- lm.y1$coefficients
predy1 <- predict(lm.y1)
residy1 <- lm.y1$residuals

ggplot(lm.mod, aes(x = X, y = Y)) +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
    # dline from prediction to 
    geom_segment(aes(xend = X[,2], yend = predy1), alpha = .2) +      
    geom_point(aes(color = abs(residy1), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "lightblue", high = "blue") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predy1), shape = 1) +
    theme_bw()

# Residuals vs Predicted Values
plot(residy1,predy1)
plot(predy1,residy1)

# Histogram plot of Error Distribution (Residuals)
predicted <- predict(lm.mod)
ggplot(data, aes(residy1)) + geom_histogram()

summary(lm.y1)
resid <- lm.mod$residuals
resid <- as.matrix(resid)
resid[,1]
plot(predicted[,1], Y[,1])
# plot residuals
residPlot <- plot(lm.mod$residuals)
summary(lm.mod)

# ======= Y1 ~ X2 ==================================
lm.y1 <- lm(data[,6] ~ data[,4])
betay1 <- lm.y1$coefficients
predy1 <- predict(lm.y1)
residy1 <- lm.y1$residuals

ggplot(data, aes(x = X3, y = Y2)) +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
    # dline from prediction to 
    geom_segment(aes(xend = X3, yend = predy1), alpha = .2) +      
    geom_point(aes(color = abs(residy1), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "lightblue", high = "blue") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predy1), shape = 1) +
    theme_bw()

# Residuals vs Predicted Values
plot(residy1,predy1)
plot(predy1,residy1)

# Histogram plot of Error Distribution (Residuals)
predicted <- predict(lm.mod)
ggplot(data, aes(residy1)) + geom_histogram()

summary(lm.y1)



# ======= Y1 ~ X1, X2, X3 ==================================
lm.y1 <- lm(data[,5] ~ data[,2:4])
betay1 <- lm.y1$coefficients
predy1 <- predict(lm.y1)
residy1 <- lm.y1$residuals

ggplot(data, aes(x = X3, y = Y2)) +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + 
    # dline from prediction to 
    geom_segment(aes(xend = X3, yend = predy1), alpha = .2) +      
    geom_point(aes(color = abs(residy1), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "lightblue", high = "blue") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predy1), shape = 1) +
    theme_bw()

# Residuals vs Predicted Values
plot(residy1,predy1)
plot(predy1,residy1)

# Histogram plot of Error Distribution (Residuals)
predicted <- predict(lm.mod)
ggplot(data, aes(residy1)) + geom_histogram()

summary(lm.y1)


resid <- lm.mod$residuals
resid <- as.matrix(resid)
resid[,1]
plot(predicted[,1], Y[,1])
# plot residuals
residPlot <- plot(lm.mod$residuals)
summary(lm.mod)

# plot regression line with residuals vs predicted values
# X1 vs Y1
ggplot(data, aes(x = X1, y = Y1)) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X1, yend = predicted[,1]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,1]), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,1]), shape = 1) +
    theme_bw()
# X1 vs Y2
ggplot(lm.mod, aes(x = X[,2], y = Y[,2])) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X[,2], yend = predicted[,2]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,2]), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,2]), shape = 1) +
    theme_bw()
# X2 vs Y1
ggplot(lm.mod, aes(x = X[,3], y = Y[,1])) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X[,3], yend = predicted[,1]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,1]), size = abs(resid[,1]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,1]), shape = 1) +
    theme_bw()
#X2 vs Y2
ggplot(lm.mod, aes(x = X[,3], y = Y[,2])) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X[,3], yend = predicted[,2]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,2]), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,2]), shape = 1) +
    theme_bw()

#X3 vs Y1
ggplot(lm.mod, aes(x = X[,4], y = Y[,1])) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X[,4], yend = predicted[,1]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,1]), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,1]), shape = 1) +
    theme_bw()

#X3 vs Y2
ggplot(lm.mod, aes(x = X[,4], y = Y[,2])) +
    geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
    geom_segment(aes(xend = X[,4], yend = predicted[,2]), alpha = .2) +      # draw line from point to line
    geom_point(aes(color = abs(resid[,2]), size = abs(resid[,2]))) +  # size of the points
    scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
    guides(color = FALSE, size = FALSE) +                             # Size legend removed
    geom_point(aes(y = predicted[,2]), shape = 1) +
    theme_bw()

summary(lm.mod, type = "hc0")

plot(X[,4],Y[,2])
