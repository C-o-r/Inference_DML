#test 1 AJR replication


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

###################### Loading functions and Data ##############################

rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


test   <- data.frame(read.csv(file="test.csv", header=TRUE, sep=","))
#df <- read.dta("Nunn_Wantchekon_AER_2011.dta")
test <- na.omit(test) 
typeof(test)
class(test)
sapply(test, class)
sapply(test, attributes)
attributes(test)
names(test)

data =test
################################ Inputs ########################################

# Outcome Variable
y  <- "trust_neighbors" #:A? log? 

# Treatment Variable
d  <- "exports"

# Instrument
#z  <- "logMort"	

#workds
# Controls
#x  <- "age + age2"       # use this for tree-based methods like forests and boosted trees
#xl <- "(age+age2)^2"   # use this for rlasso etc.

#age age2 male urban_dum i.education i.occupation i.religion i.living_conditions district_ethnic_frac frac_ethnicity_in_district i.isocode

#i.education




#i.occupation 


#i.religion 

#i.living_conditions 

    


x= "age+age2 +male "

# +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+X_Ieducation_1+                 
# X_Ieducation_2+
# X_Ieducation_3+
# X_Ieducation_4 +               
# X_Ieducation_5+
# X_Ieducation_6+
# X_Ieducation_7 +                
# X_Ieducation_8+
# X_Ieducation_9+
# X_Ioccupatio_1 +                
# X_Ioccupatio_2+
# X_Ioccupatio_3+
# X_Ioccupatio_4+
# X_Ioccupatio_5+
# X_Ioccupatio_6+
# X_Ioccupatio_7+
# X_Ioccupatio_8+
# X_Ioccupatio_9+
# X_Ioccupatio_10+
# X_Ioccupatio_11+
# X_Ioccupatio_12+
# X_Ioccupatio_13+
# X_Ioccupatio_14+
# X_Ioccupatio_15+
# X_Ioccupatio_16+
# X_Ioccupatio_18+
# X_Ioccupatio_19+
# X_Ioccupatio_20+
# X_Ioccupatio_21+
# X_Ioccupatio_22+
# X_Ioccupatio_23+
# X_Ioccupatio_24+
# X_Ioccupatio_25+
# X_Ioccupatio_995+
# X_Ireligion_2+
# X_Ireligion_3+
# X_Ireligion_4+
# X_Ireligion_5+
# X_Ireligion_6+
# X_Ireligion_7+
# X_Ireligion_10+
# X_Ireligion_11+
# X_Ireligion_12+
# X_Ireligion_13+
# X_Ireligion_14+
# X_Ireligion_15+
# X_Ireligion_360+
# X_Ireligion_361+
# X_Ireligion_362+
# X_Ireligion_363+
# X_Ireligion_995+
# X_Iliving_co_2+
# X_Iliving_co_3+
# X_Iliving_co_4+
# X_Iliving_co_5+
# X_Iisocode_2+
# X_Iisocode_3+
# X_Iisocode_4+
# X_Iisocode_5+
# X_Iisocode_6+
# X_Iisocode_7+
# X_Iisocode_8+
# X_Iisocode_9+
# X_Iisocode_10+
# X_Iisocode_11+
# X_Iisocode_12+
# X_Iisocode_13+
# X_Iisocode_14+
# X_Iisocode_15+
# X_Iisocode_16+
# X_Iisocode_17"

xl= "(age+age2 +male)^2"

# +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+X_Ieducation_1+                 
# X_Ieducation_2+
# X_Ieducation_3+
# X_Ieducation_4 +               
# X_Ieducation_5+
# X_Ieducation_6+
# X_Ieducation_7 +                
# X_Ieducation_8+
# X_Ieducation_9+
# X_Ioccupatio_1 +                
# X_Ioccupatio_2+
# X_Ioccupatio_3+
# X_Ioccupatio_4+
# X_Ioccupatio_5+
# X_Ioccupatio_6+
# X_Ioccupatio_7+
# X_Ioccupatio_8+
# X_Ioccupatio_9+
# X_Ioccupatio_10+
# X_Ioccupatio_11+
# X_Ioccupatio_12+
# X_Ioccupatio_13+
# X_Ioccupatio_14+
# X_Ioccupatio_15+
# X_Ioccupatio_16+
# X_Ioccupatio_18+
# X_Ioccupatio_19+
# X_Ioccupatio_20+
# X_Ioccupatio_21+
# X_Ioccupatio_22+
# X_Ioccupatio_23+
# X_Ioccupatio_24+
# X_Ioccupatio_25+
# X_Ioccupatio_995+
# X_Ireligion_2+
# X_Ireligion_3+
# X_Ireligion_4+
# X_Ireligion_5+
# X_Ireligion_6+
# X_Ireligion_7+
# X_Ireligion_10+
# X_Ireligion_11+
# X_Ireligion_12+
# X_Ireligion_13+
# X_Ireligion_14+
# X_Ireligion_15+
# X_Ireligion_360+
# X_Ireligion_361+
# X_Ireligion_362+
# X_Ireligion_363+
# X_Ireligion_995+
# X_Iliving_co_2+
# X_Iliving_co_3+
# X_Iliving_co_4+
# X_Iliving_co_5+
# X_Iisocode_2+
# X_Iisocode_3+
# X_Iisocode_4+
# X_Iisocode_5+
# X_Iisocode_6+
# X_Iisocode_7+
# X_Iisocode_8+
# X_Iisocode_9+
# X_Iisocode_10+
# X_Iisocode_11+
# X_Iisocode_12+
# X_Iisocode_13+
# X_Iisocode_14+
# X_Iisocode_15+
# X_Iisocode_16+
# X_Iisocode_17)^2"
# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet =Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso" , "Forest"))                       # methods for the ensemble estimation
methods      <- c("Boosting","RLasso","Nnet","Trees",  "Forest","Ensemble")          # ML methods that are used in estimation
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation

split        <- 1# number of splits



r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  print("Simulation-iteration")
  print(k)
  dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

################################ Compute Output Table ########################################

result           <- matrix(0,3, length(methods)+1)
colnames(result) <- cbind(t(methods), "best")
rownames(result) <- cbind("Median ATE", "se(median)",  "se")

result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result, digits = 2)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))