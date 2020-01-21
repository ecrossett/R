getwd()
setwd("/Users/ed/Documents/R/Statistics")
library(ggplot2)

data <-read.csv("Data.csv", header = TRUE)

head(data)
summary(data)
str(data)

PG = data[,2]
UN = data[,3]
HH = data[,4]
MKT = data[,5]
RF = data[,6]
mean(PG)

PG_RF = PG - RF
UN_RF = UN - RF
MKT_RF = MKT - RF

const = rep(1, 300)
X = cbind(const,MKT_RF)
n = 300
p = 1
# ========= Run the regression ================================================
#   RULt −Rft = α+β(Rmt −Rft)+et

# coefficient estimates, R2 and the adjusted R ̄2

betas_UN_MKT <- solve(t(X) %*% X) %*% t(X) %*% UN_RF
betas_UN_MKT

yhat = X %*% betas_UN_MKT

errors = UN_RF - yhat
# check X'e = 0
check = t(X) %*% errors
check

sigma2 = (1/298) * t(errors) %*% errors

ybar = mean(UN_RF)

SSE = sum((yhat - ybar)**2)
SST = sum((UN_RF - ybar)**2)

Rsquared = SSE / SST
Rsquared

AdjRsquared = ((1 - (1 - Rsquared)*(n-1) / (n - p - 1)))
AdjRsquared


# scatterplot of returns and CRSP-VW
#   as well as the regression line
ggplot(data, aes(x = MKT_RF, y = UN_RF)) +
    geom_jitter() +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    
    theme_bw()


# variance-covariance matrix of the OLS coefficients
#   under the assumption of homoskedasticity.
VarCov_UN_MKT<- solve(t(X) %*% X) * as.numeric(sigma2)
VarCov_UN_MKT

SE = sqrt(diag(VarCov_UN_MKT))

# t-tests to test the null hypothesis that each regression 
#     coefficient is individually equal to 0

critical_99 = qt(0.01/2, 298, lower.tail = FALSE)
critical_95 = qt(0.05/2, 298, lower.tail = FALSE)
critical_90 = qt(0.10/2, 298, lower.tail = FALSE)
# Test intercept B0 (H0: B0 = 0, H1: B0 <> 0)
Test_B0_99 = abs(betas_UN_MKT[1] / SE[1]) > critical_99
Test_B0_95 = abs(betas_UN_MKT[1] / SE[1]) > critical_95
Test_B0_90 = (betas_UN_MKT[1] / SE[1]) > critical_90
Test_B0_99
Test_B0_95
Test_B0_90
# Test intercept B1 (H0: B1 = 0, H1: B1 <> 0)
Test_B1_99 = (betas_UN_MKT[2] / SE[2]) > critical_99
Test_B1_95 = (betas_UN_MKT[2] / SE[2]) > critical_95
Test_B1_90 = (betas_UN_MKT[2] / SE[2]) > critical_90
Test_B1_99
Test_B1_95
Test_B1_90

# check for heteroskedasticity

# check that errors are centered around zero
ggplot(data, aes(x = yhat, y = errors)) + geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    xlab("Fitted Values") + ylab("Residuals")

# check for normal distribution of errors
ggplot(data, aes(x = errors)) + geom_histogram(binwidth = 0.02) + 
    xlab("Residuals")


# check linearity of errors
ggplot(data, aes(sample = errors)) + stat_qq() 


#  standard errors and the 90%, 95% and 99% confidence in- 
#     tervals under the assumption of homoskedasticity

SE = sqrt(diag(VarCov_UN_MKT))
SE
# for B0
CI_99upper = betas_UN_MKT[1] + (SE[1] * critical_99)
CI_99lower = betas_UN_MKT[1] - (SE[1] * critical_99)
CI_99upper
CI_99lower

CI_95upper = betas_UN_MKT[1] + (SE[1] * critical_95)
CI_95lower = betas_UN_MKT[1] - (SE[1] * critical_95)
CI_95upper
CI_95lower

CI_90upper = betas_UN_MKT[1] + (SE[1] * critical_90)
CI_90lower = betas_UN_MKT[1] - (SE[1] * critical_90)
CI_90upper
CI_90lower

# for B1
CI_99upper = betas_UN_MKT[2] + (SE[2] * critical_99)
CI_99lower = betas_UN_MKT[2] - (SE[2] * critical_99)
CI_99upper
CI_99lower

CI_95upper = betas_UN_MKT[2] + (SE[2] * critical_95)
CI_95lower = betas_UN_MKT[2] - (SE[2] * critical_95)
CI_95upper
CI_95lower

CI_90upper = betas_UN_MKT[2] + (SE[2] * critical_90)
CI_90lower = betas_UN_MKT[2] - (SE[2] * critical_90)
CI_90upper
CI_90lower

# variance-covariance matrix of the OLS coefficients
#     under the assumption of heteroskedasticity


D = matrix(0,300,300)
diag(D) <- errors**2

betaWhite = solve(t(X) %*%  solve(D) %*% X) %*% t(X) %*% solve(D) %*% UN_RF
betaWhite


VarCov_White <- (solve(t(X) %*% X) * (t(X) %*% D %*% X )) %*% solve(t(X) %*% X) 
VarCov_White

VarCov_UN_MKT
betas1


# standard errors and the 90%, 95% and 99% confidence in- 
#     tervals using the White variance-covariance matrix
SE
SEWhite = sqrt(diag(VarCov_White))
SEWhite
#x + (SE*critical)

# for B0
CI_99upperW = betaWhite[1] + (SEWhite[1] * critical_99)
CI_99lowerW = betaWhite[1] - (SEWhite[1] * critical_99)
CI_99upperW
CI_99lowerW

CI_95upperW = betaWhite[1] + (SEWhite[1] * critical_95)
CI_95lowerW = betaWhite[1] - (SEWhite[1] * critical_95)
CI_95upperW
CI_95lowerW

CI_90upperW = betaWhite[1] + (SEWhite[1] * critical_90)
CI_90lowerW = betaWhite[1] - (SEWhite[1] * critical_90)
CI_90upperW
CI_90lowerW

# for B1
CI_99upperW = betaWhite[2] + (SEWhite[2] * critical_99)
CI_99lowerW = betaWhite[2] - (SEWhite[2] * critical_99)
CI_99upperW
CI_99lowerW

CI_95upperW = betaWhite[2] + (SEWhite[2] * critical_95)
CI_95lowerW = betaWhite[2] - (SEWhite[2] * critical_95)
CI_95upperW
CI_95lowerW

CI_90upperW = betaWhite[2] + (SEWhite[2] * critical_90)
CI_90lowerW = betaWhite[2] - (SEWhite[2] * critical_90)
CI_90upperW
CI_90lowerW

# t-tests to test the null hypothesis that each regression 
#     coefficient is individually equal to 0 under the assumption of 
#     heteroskedasticity

# Test intercept B0 (H0: B0 = 0, H1: B0 <> 0)
Test_B0_99W = abs(betaWhite[1] / SEWhite[1]) > critical_99
Test_B0_95W = abs(betaWhite[1] / SEWhite[1]) > critical_95
Test_B0_90W = abs(betaWhite[1] / SEWhite[1]) > critical_90
Test_B0_99W
Test_B0_95W
Test_B0_90W
# Test intercept B1 (H0: B1 = 0, H1: B1 <> 0)
Test_B1_99W = abs(betaWhite[2] / SEWhite[2]) > critical_99
Test_B1_95W = abs(betaWhite[2] / SEWhite[2]) > critical_95
Test_B1_90W = abs(betaWhite[2] / SEWhite[2]) > critical_90
Test_B1_99W
Test_B1_95W
Test_B1_90W

# AIC, BIC and Hannah-Quinn ICs

regUN <- lm(UN_RF ~ MKT_RF)
summary(regUN)

AIC(regUN)
BIC(regUN)

#  Durbin-Watson and Breusch-Godfrey test statistics. 
#  QQ plots and formal tests to check whether the errors 
#     normally distributed



yhatW = X %*% betaWhite

errorsW = UN_RF - yhatW
# check X'e = 0
check = t(X) %*% errorsW
check

ggplot(data, aes(sample = errorsW)) + stat_qq() 

# check for multicollinearity


white <- lm(UN_RF ~ X)

# check for evidence against the linear model specification

# run rolling regressions with 60-month windows and plot the 
#     beta coefficients along with their 95% confidence intervals




# 2. Run the regression =========================================================
#     RULt −Rft = α+β(Rmt −Rft)+γ(RHHt −Rft)+et and repeat (a)-(o).

PG = data[,2]
UN = data[,3]
HH = data[,4]
MKT = data[,5]
RF = data[,6]
mean(PG)

PG_RF = PG - RF
UN_RF = UN - RF
MKT_RF = MKT - RF
HH_RF = HH - RF

const = rep(1, 300)
Xm = cbind(const,MKT_RF,HH_RF)
n = 300
p = 2
# 2. Run the regression ================================================


# coefficient estimates, the R2 and the adjusted R ̄2

betas_m <- solve(t(Xm) %*% Xm) %*% t(Xm) %*% UN_RF
betas_m

yhatM = Xm %*% betas_m

errorsM = UN_RF - yhatM
# check X'e = 0
checkM = t(Xm) %*% errorsM
checkM

sigma2m = (1/297) * t(errorsM) %*% errorsM

ybarM = mean(UN_RF)

SSE = sum((yhatM - ybarM)**2)
SST = sum((UN_RF - ybarM)**2)

RsquaredM = SSE / SST
RsquaredM

AdjRsquaredM = ((1 - (1 - RsquaredM)*(n-1) / (n - p - 1)))
AdjRsquaredM


# scatterplot of returns 
#   as well as the regression line
ggplot(data, aes(x = MKT_RF, y = UN_RF)) +
    geom_jitter() +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    
    theme_bw()

ggplot(data, aes(x = HH_RF, y = UN_RF)) +
    geom_jitter() +
    # regression line  
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    
    theme_bw()


# variance-covariance matrix of the OLS coefficients
#     under the assumption of homoskedasticity
VarCovM<- solve(t(Xm) %*% Xm) * as.numeric(sigma2m)
VarCovM

SE = sqrt(diag(VarCovM))
SE

# t-tests to test the null hypothesis that each regression 
#     coefficient is individually equal to 0

critical_99 = qt(0.01/2, 298, lower.tail = FALSE)
critical_95 = qt(0.05/2, 298, lower.tail = FALSE)
critical_90 = qt(0.10/2, 298, lower.tail = FALSE)
# Test intercept B0 (H0: B0 = 0, H1: B0 <> 0)
Test_B0_99 = abs(betas_m[1] / SE[1]) > critical_99
Test_B0_95 = abs(betas_m[1] / SE[1]) > critical_95
Test_B0_90 = (betas_m[1] / SE[1]) > critical_90
Test_B0_99
Test_B0_95
Test_B0_90
# Test intercept B1 (H0: B1 = 0, H1: B1 <> 0)
Test_B1_99 = (betas_m[2] / SE[2]) > critical_99
Test_B1_95 = (betas_m[2] / SE[2]) > critical_95
Test_B1_90 = (betas_m[2] / SE[2]) > critical_90
Test_B1_99
Test_B1_95
Test_B1_90

# check for heteroskedasticity

# check that errors are centered around zero
ggplot(data, aes(x = yhatM, y = errorsM)) + geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    xlab("Fitted Values") + ylab("Residuals")

# check for normal distribution of errors
ggplot(data, aes(x = errorsM)) + geom_histogram(binwidth = 0.02) + 
    xlab("Residuals")


# check linearity of errors
ggplot(data, aes(sample = errorsM)) + stat_qq() 


# standard errors and the 90%, 95% and 99% confidence in- 
#     tervals under the assumption of homoskedasticity

SEm = sqrt(diag(VarCovM))
SEm
# for B0
CI_99upper = betas_m[1] + (SEm[1] * critical_99)
CI_99lower = betas_m[1] - (SEm[1] * critical_99)
CI_99upper
CI_99lower

CI_95upper = betas_m[1] + (SEm[1] * critical_95)
CI_95lower = betas_m[1] - (SEm[1] * critical_95)
CI_95upper
CI_95lower

CI_90upper = betas_m[1] + (SEm[1] * critical_90)
CI_90lower = betas_m[1] - (SEm[1] * critical_90)
CI_90upper
CI_90lower

# for B1
CI_99upper = betas_m[2] + (SEm[2] * critical_99)
CI_99lower = betas_m[2] - (SEm[2] * critical_99)
CI_99upper
CI_99lower

CI_95upper = betas_m[2] + (SEm[2] * critical_95)
CI_95lower = betas_m[2] - (SEm[2] * critical_95)
CI_95upper
CI_95lower

CI_90upper = betas_m[2] + (SEm[2] * critical_90)
CI_90lower = betas_m[2] - (SEm[2] * critical_90)
CI_90upper
CI_90lower

# variance-covariance matrix of the OLS coefficients
#     under the assumption of heteroskedasticity


Dm = matrix(0,300,300)
diag(Dm) <- errorsM**2

betaWhiteM = solve(t(Xm) %*%  solve(Dm) %*% Xm) %*% t(Xm) %*% solve(Dm) %*% UN_RF
betaWhiteM


VarCov_WhiteM <- (solve(t(Xm) %*% Xm) * (t(Xm) %*% Dm %*% Xm )) %*% solve(t(Xm) %*% Xm) 
VarCov_WhiteM

VarCov_UN_MKT
betas1


# standard errors and the 90%, 95% and 99% confidence in- 
#     tervals using the White variance-covariance matrix
SE
SEWhiteM = sqrt(diag(VarCov_WhiteM))
SEWhiteM
#x + (SE*critical)

# for B0
CI_99upperW = beta_m[1] + (SEWhiteM[1] * critical_99)
CI_99lowerW = beta_m[1] - (SEWhiteM[1] * critical_99)
CI_99upperW
CI_99lowerW

CI_95upperW = beta_m[1] + (SEWhiteM[1] * critical_95)
CI_95lowerW = beta_m[1] - (SEWhiteM[1] * critical_95)
CI_95upperW
CI_95lowerW

CI_90upperW = beta_m[1] + (SEWhiteM[1] * critical_90)
CI_90lowerW = beta_m[1] - (SEWhiteM[1] * critical_90)
CI_90upperW
CI_90lowerW

# for B1
CI_99upperW = beta_m[2] + (SEWhiteM[2] * critical_99)
CI_99lowerW = beta_m[2] - (SEWhiteM[2] * critical_99)
CI_99upperW
CI_99lowerW

CI_95upperW = beta_m[2] + (SEWhiteM[2] * critical_95)
CI_95lowerW = beta_m[2] - (SEWhiteM[2] * critical_95)
CI_95upperW
CI_95lowerW

CI_90upperW = beta_m[2] + (SEWhiteM[2] * critical_90)
CI_90lowerW = beta_m[2] - (SEWhiteM[2] * critical_90)
CI_90upperW
CI_90lowerW

# t-tests to test the null hypothesis that each regression 
#     coefficient is individually equal to 0 under the assumption of 
#     heteroskedasticity

# Test intercept B0 (H0: B0 = 0, H1: B0 <> 0)
Test_B0_99W = abs(betaWhiteM[1] / SEWhiteM[1]) > critical_99
Test_B0_95W = abs(betaWhiteM[1] / SEWhiteM[1]) > critical_95
Test_B0_90W = abs(betaWhiteM[1] / SEWhiteM[1]) > critical_90
Test_B0_99W
Test_B0_95W
Test_B0_90W
# Test intercept B1 (H0: B1 = 0, H1: B1 <> 0)
Test_B1_99W = abs(betaWhiteM[2] / SEWhiteM[2]) > critical_99
Test_B1_95W = abs(betaWhiteM[2] / SEWhiteM[2]) > critical_95
Test_B1_90W = abs(betaWhiteM[2] / SEWhiteM[2]) > critical_90
Test_B1_99W
Test_B1_95W
Test_B1_90W

# AIC, BIC and Hannah-Quinn ICs

regM <- lm(UN_RF ~ Xm)
summary(regM)

AIC(regM)
BIC(regM)

AIC(regUN)
BIC(regUN)
# Durbin-Watson and Breusch-Godfrey test statistics
# QQ plots and formal tests to check whether the errors 
#     normally distributed



yhatW = X %*% betaWhite

errorsW = UN_RF - yhatW
# check X'e = 0
check = t(X) %*% errorsW
check

ggplot(data, aes(sample = errorsW)) + stat_qq() 

# check for multicollinearity


white <- lm(UN_RF ~ X)

# regression (check)
regUN <- lm(UN_RF ~ MKT_RF)
summary(regUN)
plot(MKT, UN)
regUN$coefficients

multi <- lm(UN_RF ~ Xm)
summary(multi)

ggplot(data = regUN, aes(x = .fitted, y = .resid)) + geom_point() + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    xlab("Fitted Values") + ylab("Residuals")

ggplot(data = regUN, aes(x = .resid)) + geom_histogram(binwidth = 0.02) + 
    xlab("Residuals")

ggplot(data = regUN, aes(sample = .resid)) + stat_qq() 
