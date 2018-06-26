#1



library(MASS);
library(rpart);
library(doParallel)
library(sandwich);
library(hdm);
library(randomForest);
library(foreign);
library(quantreg);
library(mnormt);
library(gbm);
library(glmnet);
library(nnet)
library(matrixStats)
library(quadprog)
library(ivmodel)
library(xtable)
#install.packages("readstata13")

library(readstata13)




setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")

rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")  #specify the number of cores to use.
#data  <- read.dta("slave1.dta");
data <- read.dta13("slave1.dta")
data = data[1:500,]

Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")
arguments    <- list( Trees = Trees, Forest=Forest, Nnet=Nnet,RLasso=RLasso)
#ensemble     <- list(methods=c( "Trees", "Forest")  )            # specify methods for the ensemble estimation
#methods      <- c("RLasso","Trees", "Forest", "Boosting","Nnet", "Ensemble")         # method names to be estimated
methods      <- c("Trees", "Forest","Nnet","RLasso")         # method names to be estimated
split        <- 2     

############## Arguments for DoubleML function:
# data:     : data matrixsource('C:/Users/cor64/Desktop/Uni/Erasmus/Thesis/DMLonGitHub-master/1.R', echo=TRUE)

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
y      <- "trust_neighbors";
# Treatment Indicator
d      <- "exports";  
# Controls

#baseline_controls "age age2 male urban_dum i.education i.occupation i.religion i.living_conditions district_ethnic_frac frac_ethnicity_in_district i.isocode"
x      <- "age +age2+ male+ urban_dum+  district_ethnic_frac+ frac_ethnicity_in_district " ;
# use this for tree-based methods like forests and boosted trees
xl     <- "( age +age2+ male+ urban_dum+  district_ethnic_frac+ frac_ethnicity_in_district)^2";  
# use this for rlasso etc.




r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}




result           <- matrix(0,3, length(methods)+1)
colnames(result) <- cbind(t(methods), "best")
rownames(result) <- cbind("Median ATE", "se(median)",  "se")

r= as.matrix(r)


result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result[2,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
result[3,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)

result_table <- round(result, digits = 0)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))




