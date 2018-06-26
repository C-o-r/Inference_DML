#Complexity Analysis of DML techniques
#Alexander Kiel #386879
#Boosting
# a) deviate input size and compare algorithms's perfomances
setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")
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

library(tictoc)

library(plotrix)
library(RColorBrewer)

# create plots for R
# x <- c(1:5); y <- x # create some data
# par(pch=22, col="red") # plotting symbol and color
# par(mfrow=c(3,5)) # all plots on one page
# opts = c("p","l","o","b","c","s","S","h")
# par(mar = rep(3, 5))
# for(i in 1:length(opts)){
#   heading = paste("type=",opts[i])
#   plot(x, y, type="n", main=heading)
#   lines(x, y, type=opts[i])
# }


# Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
# Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
# RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
# Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
# Trees        <- list(reg_method="anova", clas_method="class")
# 
# arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)
# 
# ensemble     <- list(methods=c("RLasso", "Boosting", "Forest", "Nnet"))              # specify methods for the ensemble estimation
# methods      <- c("RLasso","Trees", "Forest", "Boosting","Nnet", "Ensemble")         # method names to be estimated
# split        <- 2                                                               # number of splits

# RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)

#load stuff

rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######
#code in order to make account of splits
# increase size

# still change
n <- 10
f=do.call("rbind", replicate(n, AJR, simplify = FALSE))

#Rlasso
####
tic.clearlog()
for (i in 60:200){
  
  data =f[1:i,]
  
  
  
  #####
  
  
  ############################### Inputs ########################################
  
  # Outcome Variable
  y  <- "GDP" #:A? log? 
  
  # Treatment Variable
  d  <- "Exprop"
  
  # Instrument
  z  <- "logMort"	
  
  # Controls
  x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
  xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.
  
  
  
  ############## Arguments for DoubleML function:
  
  # data:     : data matrix
  # y         : outcome variable
  # d         : treatment variable
  # z         : instrument
  # xx        : controls for tree-based methods
  # xL        : controls for penalized linear methods
  # methods   : machine learning methods
  # DML       : DML1 or DML2 estimation (DML1, DML2)
  # nfold     : number of folds in cross fitting
  # est       : estimation methods (IV, LATE, plinear, interactive)
  # arguments : argument list for machine learning methods
  # ensemble  : ML methods used in ensemble method
  # silent    : whether to print messages
  # trim      : bounds for propensity score trimming
  split        <- 2  
  
  
  Boosting     <- list(bag.fraction = 1, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
  methods      <- c("Boosting")         # method names to be estimated
  arguments    <- list( Boosting=Boosting)
  
  
  
  
  
  
  
  
  
  
  tic(i)
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  toc(log = TRUE, quiet = TRUE)
  print(i)
}

log.txt <- tic.log(format = TRUE)
log.lst <- tic.log(format = FALSE)
tic.clearlog()
timings <- unlist(lapply(log.lst, function(x) x$toc - x$tic))
mean(timings)
# [1] 1.001
writeLines(unlist(log.txt))

#plot timings

x <- c(60:200); 
y <- as.numeric(timings) # create some data

my_colors = brewer.pal(8, "Set2") 



# Plot x and y
par( mfrow = c( 1, 1 ) )
clplot(x,y ,
       ylab="time of Boosting",xlab	="amount of observations", main="", lwd=5, levels=c(1,2,3,4,5), col=my_colors, showcuts=T , bty="n")

write.csv(y, file = "c1_Boosting_60_200.csv", row.names = FALSE)
