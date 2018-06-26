# Econometrics Seminar-2018
# 02.06.18

# Table of Content
#
# @Prerequisites
# Data, Stat regressions
#
# @Replication Table 1
#
# @Replication Table 2
#
# @Replication Table 3
#
# @Replication Table 5
#
# @Replication Table 6
#
# @Replication Table 9
#
# @Replication Table 10

###########################


#@ Prerequisites#######################################################

Sys.info()  
#disable scientific notation
options(scipen=999)


#Necessary Libraries
library(hdm)
library(foreign);
library(quantreg);
library(mnormt);
library(gbm);
library(glmnet);
library(MASS);
library(rpart);
library(doParallel)
library(sandwich);
library(hdm);
library(randomForest);
library(nnet)
library(matrixStats)
library(quadprog)
library(xtable)
library(ivmodel)
require(gdata)
library(xlsx)

######################

#change working directory
setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")

#read Data
df = read.xls ("Slave.xls", sheet = 1, header = TRUE)
summary(df)
#adjust data for missing values (NA)
#df[is.na(df)] <- 0  # is this correct? does not retrieve correct STATA regression results; but na is not allowed for DML Im afraid
#df <- na.omit(df) # this one reduces the amount of observations to 7000?

#check
source("AJR.R")
#check
source("Bonus.R")
#check
source("401K.R")
#check
source("401K-Late.R")

test   <- data.frame(read.csv(file="test.csv", header=TRUE, sep=","))
#df <- read.dta("Nunn_Wantchekon_AER_2011.dta")
df <- na.omit(test) 
typeof(test)
class(test)
sapply(test, class)
sapply(test, attributes)
attributes(test)
names(test)



require(stats)  # normally loaded
loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
require(stats)  # normally loaded
loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
abline(h = pretty(rx, 10), v = pretty(ry, 10), col = "lightgray")
text(x, y, labels(eurodist), cex = 0.8)

