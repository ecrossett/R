
## ========================== RETURNS ANALYSIS =================================
# This program reads data downloaded from a database of historical stock prices
# from Jan1994 - Dec 2013.. 
# First the data is manipulated into appropriate formats, and then the program
# used the data to create various charts and tables.  Namely:
#   - Matrix of mean, sd, Sharpe Ratio, Skew, and Kurtosis stocks
#       compared to those of a sector index and the general market
#   - Covariance matrix for the above inputs
#   - Correlation matrix for the above inputs
#   - Time series charts of monthly returns
#   - Histogram distribution of monthly returns
#   - 3D Plots of Joint distribution between above inputs
#   - 2d Plots of Joint distribution with marginal histogram distribution overlays

stocks <- read.csv("dataSet2.csv" , header = TRUE)
summary(stocks)
dim(stocks)
head(stocks)
# Load pachakges
install.packages("ggExtra")
install.packages("hexbin")
install.packages("plot3D")
install.packages("moments")
install.packages("lubridate")
library(lubridate)
library(moments)
library(plot3D)
library(hexbin)
library(ggExtra)
library(tidyverse)
library(readr)
library(ggplot2)
library(statsr)
library(dplyr)
# Create data frame
stockDF <- as.data.frame(stocks)
str(stockDF)
head(stockDF)

# Create columns for risk-adjusted returns
stockDF2 <- stockDF %>%
    #filter(Date,format.Date(Date,"%Y") != 2015) %>%
    mutate(mkt_rf = stockDF$MKT - stockDF$RF) %>%
    mutate(hh_rf = stockDF$HH - stockDF$RF) %>%
    mutate(pg_rf = stockDF$PG - stockDF$RF) %>%
    mutate(un_rf = stockDF$UN - stockDF$RF) %>%
    mutate(rf_rf = stockDF$RF - stockDF$RF) 
    class(stockDF[1,1])
# Create variables for risk-adjusted returns
mkt_rf <- stockDF2$mkt_rf
hh_rf <- stockDF2$hh_rf
pg_rf <- stockDF2$pg_rf
un_rf<- stockDF2$un_rf
rf_rf <- stockDF2$rf_rf
head(stockDF2)
# Create subset for moment table
moments <- stockDF2[,7:10]
moments[,5] <- stockDF2[6]
head(moments)
    
# Compute means, SD, Sharpe
means <- sapply(moments, function(x) round((mean(x)*100), 2))
SD <- sapply(moments, function(x) round((sd(x)*100), 2))
momentsS <-moments
momentsS[,5] <- 0
meansS <- sapply(momentsS, function(x) round((mean(x)*100), 2))
Sharpe_Ratio <- round((meansS / SD),2)
skew <- sapply(moments, function(x) round(skewness(x), 2))
kurt <- sapply(moments, function(x) round(kurtosis(x), 2))
# Remove headings
means <- unname(means)
SD <- unname(SD)
Sharpe <- unname(Sharpe_Ratio)
skew <- unname(skew)
kurt <- unname(kurt)
# Create matrix
Moments <- matrix(c(means,SD,Sharpe, skew,kurt),5,5)
rownames(Moments) <- c("Mkt-RF","HH-RF", "PG-RF", "UN-RF","RF")
colnames(Moments) <- c("Mean","SD", "Sharpe Ratio", "Skew","Kurt")

# Return value of moments
Moments

# Create dataframe for covariance matrix
covariance <- stockDF2[,7:10]
covariance <- round(cov(covariance)*100,2)
rownames(covariance) <- c("Mkt-RF","HH-RF", "PG-RF", "UN-RF")
colnames(covariance) <- c("Mkt-RF","HH-RF", "PG-RF", "UN-RF")

# Return covariance matrix
covariance

# Create dataframe for correlation matrix
correl <- stockDF2[,7:10]
correl <- round(cor(correl),2)
rownames(correl) <- c("Mkt-RF","HH-RF", "PG-RF", "UN-RF")
colnames(correl) <- c("Mkt-RF","HH-RF", "PG-RF", "UN-RF")

# Return correlation matrix
correl

#dates <- as.Date(stockDF[,1], format ="%m/%d/%Y")

dates1 <- stockDF[,1]
dates1 <- c("Jan-94","Aug-95","Apr-97", "Dec-98", "Aug-00", "Apr-02", "Feb-03", "Dec-03", "Aug-05", "Apr-07","Dec-08","Aug-10", "Apr-12", "Dec-13")
dates1 <- dates1[c(200,220,240)]

## ================================= Time Series Returns Charts =============================================
# Returns histogram of PG
pg <- ggplot(data = stockDF, aes(x = Date, y = stockDF$PG, group = 1)) 
pg + 
    geom_line(color = "lightblue", size = 0.5) + 
    labs(x = "Jan-94   Aug-95   Apr-97   Dec-98   Aug-00   Apr-02   Feb-03   Dec-03   Aug-05   Apr-07   Dec-08   Aug-10   Apr-12   Dec-13", y = "Return") +
    ylim(-0.4, 0.4) + ggtitle("Returns\n", "From Jan 1994 to Dec 2013") + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank())

# Returns histogram of UN
un <- ggplot(data = stockDF, aes(x = Date, y = stocks$UN, group = 1))
un + geom_line(color = "lightblue", size = 0.5) + labs(x =  "Jan-94   Aug-95   Apr-97   Dec-98   Aug-00   Apr-02   Feb-03   Dec-03   Aug-05   Apr-07   Dec-08   Aug-10   Apr-12   Dec-13", y = "Return") +
    ylim(-0.4, 0.4) + ggtitle("Returns") + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank())

# Returns histogram of HH
hh <- ggplot(data = stockDF, aes(x = Date, y = hh_rf, group = 1))
hh + geom_line(color = "lightblue", size = 0.5) + labs(x =  "Jan-94   Aug-95   Apr-97   Dec-98   Aug-00   Apr-02   Feb-03   Dec-03   Aug-05   Apr-07   Dec-08   Aug-10   Apr-12   Dec-13", y = "Return") +
    ylim(-0.4, 0.4) + ggtitle("HH-RF Returns") + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank())

# Returns histogram of MKT
mkt <- ggplot(data = stockDF, aes(x = Date, y = mkt_rf, group = 1))
mkt + geom_line(color = "lightblue", size = 0.5) + labs(x =  "Jan-94   Aug-95   Apr-97   Dec-98   Aug-00   Apr-02   Feb-03   Dec-03   Aug-05   Apr-07   Dec-08   Aug-10   Apr-12   Dec-13", y = "Return") +
    ylim(-0.4, 0.4) + ggtitle("Mkt-RF Returns") + 
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank())


## ================================= RETURNS HISTOGRAM CHARTS =============================================
# Returns histogram of PG
pghist <- ggplot(data = stockDF, aes(x = stockDF$PG))
pghist + geom_histogram(binwidth = .01, color = "white", fill = "lightblue") + 
    labs(x = "Return", y = "") + ylim(0, 35) + xlim(-0.4, 0.4) + 
    ggtitle("Distribution of Returns") + 
    theme(plot.title = element_text(hjust = 0.5))
# Returns histogram of UN
unhist <- ggplot(data = stockDF, aes(x = stockDF$UN))
unhist + geom_histogram(binwidth = .01, color = "white", fill = "lightblue") + 
    labs(x = "Return", y = "") + ylim(0, 35) + xlim(-0.4, 0.4) + 
    ggtitle("Distribution of Returns") + 
    theme(plot.title = element_text(hjust = 0.5))

# Returns histogram of HH
hhhist <- ggplot(data = stockDF, aes(x = hh_rf))
hhhist + geom_histogram(binwidth = .0069, color = "white", fill = "lightblue") + 
    labs(x = "Return", y = "") + ylim(0, 35) + xlim(-0.4, 0.4) + 
    ggtitle("Distribution of Households-Rf Returns") + theme(plot.title = element_text(hjust = 0.5))

# Returns Histogram of MKT
mkthist <- ggplot(data = stockDF, aes(x = mkt_rf))
mkthist + geom_histogram(binwidth = .006, color = "white", fill = "lightblue") + 
    labs(x = "Return", y = "") + ylim(0, 35) + xlim(-0.4, 0.4) + 
    ggtitle("Distribution of Mkt-Rf Returns") + 
    theme(plot.title = element_text(hjust = 0.5))

## ======================== 2D JOINT DISTRIBUTION WITH MARGINAL HISTOGRAMS CHARTS ================================

# Joint Distribution with Marginal Histograms: PG vs MKT
joint_pgMKT <- ggplot(data = stockDF, aes(x = stockDF$MKT, y = stockDF$PG))+
    geom_hex(color = "blue") + 
    labs(x = "Mkt Return", y = "PG Return") +
    ggtitle("Joint Distribution of Mkt and Procter & Gamble Returns") + 
    theme(plot.title = element_text(hjust = 0.5))
ggExtra::ggMarginal(joint_pgMKT, x = stockDF$MKT, y = stockDF$PG, type = "histogram", margins = "both", size = 5)
plot3D::hist3D(joint_pgMKT,z = density(stockDF$MKT))

# Joint Distribution with Markginal Histogram: UNI vs MKT
joint_Uni <- ggplot(data = stockDF, aes(x = stockDF$MKT, y = stockDF$UN))+
    geom_hex(color = "blue") + 
    labs(x = "Mkt Return", y = "Unilever Return") +
    ggtitle("Joint Distribution of Mkt and Unilever Returns") + 
    theme(plot.title = element_text(hjust = 0.5))
ggExtra::ggMarginal(joint_Uni, x = stockDF$MKT, y = stockDF$UN, type = "histogram", margins = "both", size = 5)

# Joint Distribution with Marginal Histogram: HH vs MKT
joint_HH <- ggplot(data = stockDF, aes(x = stockDF$MKT, y = stockDF$HH))+
    geom_hex(color = "blue") + 
    labs(x = "Mkt Return", y = "HH Return") +
    ggtitle("Joint Distribution of Mkt and HH Returns") + 
    theme(plot.title = element_text(hjust = 0.5))
ggExtra::ggMarginal(joint_HH, x = stockDF$MKT, y = stockDF$UN, type = "histogram", margins = "both", size = 5)


## ======================== 3D Joint Distribution Histograms =============================================

##  3D Joint Distribution for PG vs MKT:
x <- stockDF$MKT
y <- stockDF$PG
##  Create cuts:
x_c <- cut(x, 30)
y_c <- cut(y, 30)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, border="black", col = "lightblue")


## 3D Joint Distribution for UN vs MKT:
x <- stockDF$MKT
y <- stockDF$UN
##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, border="black", col = "lightblue")

##  3D Joint Distribution for HH vs MKT:
x <- stockDF$MKT
y <- stockDF$HH
##  Create cuts:
x_c <- cut(x, 20)
y_c <- cut(y, 20)
##  Calculate joint counts at cut levels:
z <- table(x_c, y_c)
##  Plot as a 3D histogram:
hist3D(z=z, border="black", col = "lightblue")

