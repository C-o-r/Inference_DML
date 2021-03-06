# Table 1
# Replica + DML


df   <- read.csv(file="test.csv", header=TRUE, sep=",")
#df <- read.dta("Nunn_Wantchekon_AER_2011.dta")
df <- na.omit(df) 

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


##########################################################
#PART1: Replica in R
##########################################################


#baseline_controls:
#age 
#age2 
#male
#urban_dum 
#i.education 
#i.occupation 
#i.religion 
#i.living_conditions 
#district_ethnic_frac 
#frac_ethnicity_in_district 
#i.isocode"

#colonial_controls: 
#malaria_ecology 
#total_missions_area
#explorer_contact 
#railway_contact 
#cities_1400_dum 
#i.v30 
#v33

#DATA

Y <- df$trust_neighbors
D=df$exports
D1= df$exports
D2= df$export_area
D3= df$export_pop
D4= df$ln_exports
D5= df$ln_export_area
D6= df$ln_export_pop
age = df$age
age2 = df$age2
male = df$male
urban_dum =df$urban_dum
district_ethnic_frac=df$district_ethnic_frac
frac_ethnicity_in_district=df$frac_ethnicity_in_district


# construct for those variables dummies ( in excel file those have one column but in Stata these are binary dummies ???)
#  i.education 
#  i.occupation 
#  i.religion 
#  i.living_conditions 
#  i.isocode

baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]
education=df[ , c("education")]
df$education_fac= factor(df[ , c("education")])
occupation=df[ , c("occupation")]
occupation_fac=factor(df[ , c("occupation")])
religion=df[ , c("religion")]
religion_fac=factor(df[ , c("religion")])
living_conditions=df[ , c("living_conditions")]
living_conditions_fac=factor(df[ , c("living_conditions")])
isocode=df[ , c("isocode")]

#COEFFIECENT REPLICATION

#Table 1_Slave_exports
test_regression1 <- lm( Y~ D1+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression1) 
# works okayish ~-0.00067914 vs. -0.00068
# R^2 = 0.16 vs. 0.1558 (unadjusted, adjusted even smaller 0.1526 (( )))
# std error stil false 0.00004855 vs ~ 0.000013 -> use clustering ? 

#Table 1_Exports/area
test_regression2 <- lm( Y~ D2+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression2) 

#Table 1_Exports/histrical pop
test_regression3 <- lm( Y~ D3+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression3) 

#Table 1_ln(1+exports)
test_regression4 <- lm( Y~ D4+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression4) 

#Table 1_ln(1+exports/area)
test_regression5 <- lm( Y~ D5+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression5) 

#Table 1_ln(1+exports/historical pop)
test_regression6 <- lm( Y~ D6+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression6) 


# final Table 1 with correctly estimated coefficients and R^2
result <- matrix(0,3, 6)
result[1,1]=round(summary(test_regression1)$coefficients[2, 1], digits =5)
result[1,2]=round(summary(test_regression2)$coefficients[2, 1] , digits =3)
result[1,3]=round(summary(test_regression3)$coefficients[2, 1] , digits =3)
result[1,4]=round(summary(test_regression4)$coefficients[2, 1], digits =3)
result[1,5]=round(summary(test_regression5)$coefficients[2, 1], digits =3)
result[1,6]=round(summary(test_regression6)$coefficients[2, 1], digits =3)
result[2,1]=round(summary(test_regression1)$r.squared, digits =2)
result[2,2]=round(summary(test_regression2)$r.squared , digits =2)
result[2,3]=round(summary(test_regression3)$r.squared , digits =2)
result[2,4]=round(summary(test_regression4)$r.squared , digits =2)
result[2,5]=round(summary(test_regression5)$r.squared , digits =2)
result[2,6]=round(summary(test_regression6)$r.squared , digits =2)

colnames(result) <- cbind("Slave exports (thousands)","Exports/area","Exports/historical pop","ln(1+exports)","ln(1+exports/area)","ln(1+exports/historical pop")
rownames(result) <- cbind("traditional estimated coefficient", "R-squared", "DML coefficient")

result_table <- round(result, digits = 6)


for(i in 1:ncol(result_table)){
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste(result_table[j,i], sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste( result_table[j,i], sep="")
    
  }
}
print(xtable(result_table, digits=4))
print(result_table)

##########################################################
#PART2:DML
##########################################################

# create dataset for DML Algorithm

#data1 <- data.frame(Y,D1,D2,D3,D4,D5,D6,age,age2,male,urban_dum,district_ethnic_frac,education_fac,occupation_fac,religion_fac,living_conditions_fac,isocode)
#data1 <-data.frame(Y,D1,age,age2)
#data1_na <- na.omit(data1) # 20325 from 21822
#data1_na_short <-  data1_na[1:1000,]



################ Loading functions and Data ########################


#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1210);
cl <- makeCluster(12, outfile="")


# Outcome Variable
y      <- "trust_neighbors";

# Treatment Indicator
d      <- "exports";  


#female+black+othrace+ dep+q2+q3+q4+q5+q6+agelt35+agegt54+durable+lusd+husd
#######################################################################################################################
#Full computation Table 1 so far invloving all controls NOT adjusted categorically!!!; 

# 1. Slave exports

#CORRECT ONE; DOESNT WORK FOR NOW 
#data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac, occupation_fac, religion_fac, education_fac,living_conditions,isocode)

#USE THIS ONE INSTEAD; WHICH DOES NOT ACCOUNT FOR CAT. VARIABLES 
data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na

data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na

#+education+occupation+religion+living_conditions

x      <- "age+age2"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age)^2";     # use this for rlasso etc.

Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  #print(k)
  dml <- DoubleML(data=df, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result1           <- matrix(0,3, length(methods)+1)
colnames(result1) <- cbind(t(methods), "best")
rownames(result1) <- cbind("Median ATE", "se(median)",  "se")

result1[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result1[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result1[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

############################################################################################################################
######################################################################################################################

# 2.Exports/area
data2 <-data.frame(Y,D2,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data2_na <- na.omit(data2) 
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D2";  
data2_na_short <-  data2_na

x2      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl2     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits
# number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data2_na_short, y=y, d=d, z=NULL, xx=x2, xL=xl2, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result2           <- matrix(0,3, length(methods)+1)
colnames(result2) <- cbind(t(methods), "best")
rownames(result2) <- cbind("Median ATE", "se(median)",  "se")

result2[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result2[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result2[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)


##########################################################################################################################

# 3. Exports/historical pop
data3 <-data.frame(Y,D3,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data3_na <- na.omit(data3) 
data3_na_short <-  data3_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D3";  
x3      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl3    <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.


Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits
# number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data3_na_short, y=y, d=d, z=NULL, xx=x3, xL=xl3, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result3           <- matrix(0,3, length(methods)+1)
colnames(result3) <- cbind(t(methods), "best")
rownames(result3) <- cbind("Median ATE", "se(median)",  "se")

result3[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result3[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result3[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)



##############################################################################################################

# 4. ln(1+exports)
data4 <-data.frame(Y,D4,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data4_na <- na.omit(data4) 
data4_na_short <-  data4_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D4";  
x4      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl4     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits
# number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data4_na_short, y=y, d=d, z=NULL, xx=x4, xL=xl4, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result4          <- matrix(0,3, length(methods)+1)
colnames(result4) <- cbind(t(methods), "best")
rownames(result4) <- cbind("Median ATE", "se(median)",  "se")

result4[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result4[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result4[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)




########################################################################################################################################

# 5. ln(1+exports/area)
data5 <-data.frame(Y,D5,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data5_na <- na.omit(data5) 
data5_na_short <-  data5_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D5";  
x5     <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl5     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.


Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits
# number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data5_na_short, y=y, d=d, z=NULL, xx=x5, xL=xl5, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result5          <- matrix(0,3, length(methods)+1)
colnames(result5) <- cbind(t(methods), "best")
rownames(result5) <- cbind("Median ATE", "se(median)",  "se")

result5[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result5[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result5[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

##########################################################################################################################


# 6. ln(1+exports/historical pop)
data6 <-data.frame(Y,D6,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data6_na <- na.omit(data6) 
data6_na_short <-  data6_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D6";  
x6      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl6     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees) #Boosting=Boosting

ensemble     <- list(methods=c("Boosting","RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble", "Boosting")         # method names to be estimated
split        <- 2                                                               # number of splits
# number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data6_na_short, y=y, d=d, z=NULL, xx=x6, xL=xl6, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result6          <- matrix(0,3, length(methods)+1)
colnames(result6) <- cbind(t(methods), "best")
rownames(result6) <- cbind("Median ATE", "se(median)",  "se")

result6[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result6[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result6[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

################################################
################################################

