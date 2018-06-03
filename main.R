# Econometrics Seminar-2018
# 01.06.18

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

#adjust data for missing values (NA)

df <- na.omit(df)



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


#@TABLE 1
#test data set regression; replicate Table 1

#reg trust_neighbors exports `baseline_controls', 

Y <- df$trust_neighbors
D=df$exports

D1= df$exports

D2= df$export_area

D3= df$export_pop

D4= df$ln_exports

D5= df$ln_export_area

D6= df$ln_export_pop

#baline_controls=df[,c(1,2,5)]
baline_controls=df[ , c("age","age2","male","urban_dum","district_ethnic_frac","frac_ethnicity_in_district")]

age = df$age
age2 = df$age2
male = df$male
urban_dum =df$urban_dum
district_ethnic_frac=df$district_ethnic_frac
frac_ethnicity_in_district=df$frac_ethnicity_in_district

baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]

# construct for those variables dummies ( in excel file those have one column but in Stata these are binary dummies ???)
#  i.education 
#  i.occupation 
#  i.religion 
#  i.living_conditions 
#  i.isocode



  
#"age age2 male urban_dum i.education i.occupation i.religion i.living_conditions district_ethnic_frac frac_ethnicity_in_district i.isocode"



summary(df)


#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district, data=df)
#summary(test_regression) 

# -0.00061982368 parameter deviates from ???0.00068

# try simplty with those regressors; FAIL

#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 

# -0.00076440340 parameter deviates from ???0.00068

# works a bit; close to D 
test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
summary(test_regression) 
# -0.00076440340 parameter deviates from ???0.00068 exlucing several dummies


#build Dummies for those variables like in Stata file:
baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]
education=df[ , c("education")]
education_fac= factor(df[ , c("education")])
occupation=df[ , c("occupation")]
occupation_fac=factor(df[ , c("occupation")])
religion=df[ , c("religion")]
religion_fac=factor(df[ , c("religion")])
living_conditions=df[ , c("living_conditions")]
living_conditions_fac=factor(df[ , c("living_conditions")])
isocode=df[ , c("isocode")]





test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions+isocode, data=df)
summary(test_regression) 



#@ignore
#create a dummy for each value of education occupation religion living_conditions and isode_code
#educ_0 <- as.numeric(education == 0)
#educ_1 <- as.numeric(education == 1)
#educ_2 <- as.numeric(education == 2)
#educ_3 <- as.numeric(education == 3)
#educ_4 <- as.numeric(education == 4)
#educ_5 <- as.numeric(education == 5)
#educ_6 <- as.numeric(education == 6)
#educ_7 <- as.numeric(education == 7)
#educ_8 <- as.numeric(education == 8)
#educ_9 <- as.numeric(education == 9)
#educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9 +occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 



#new trial with factored stuff 
#Table 1_1
test_regression1 <- lm( Y~ D1+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression1) 
# works okayish ~-0.00067914 vs. -0.00068
# R^2 = 0.16 vs. 0.1558 (unadjusted, adjusted even smaller 0.1526 (( )))
# std error stil false 0.00004855 vs ~ 0.0013


#Table 1_2
test_regression2 <- lm( Y~ D2+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression2) 

#Table 1_3
test_regression3 <- lm( Y~ D3+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression3) 

#Table 1_4
test_regression4 <- lm( Y~ D4+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression4) 

#Table 1_5
test_regression5 <- lm( Y~ D5+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression5) 

#Table 1_6
test_regression6 <- lm( Y~ D6+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression6) 


# create dataset for replication Table 1

data1 <- data.frame(Y,D1,D2,D3,D4,D5,D6,age,age2,male,urban_dum,district_ethnic_frac,education_fac,occupation_fac,religion_fac,living_conditions_fac,isocode)

data1_1 <-data.frame(Y,D1,age,age2)

#data1= data1[1:50,]
write.xlsx(data1_1, "c:/Users/cor64/Documents/data1_1.xlsx")

#write.dta(data1, "C:/data1.dta")

#write.xls(data1)



# @Replication Table 1_1###########################################################


#### Loading functions and Data #######

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")
#data1 = read.xls ("c:/Users/cor64/Documents/data1.xlsx", sheet = 1, header = TRUE)

################################ Inputs ########################################

# Outcome Variable
y <- "Y"

# Treatment Variable
d  <- "D1"

# Controls
#x <- "age+age2 +male +urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode" # use this for tree-based methods like forests and boosted trees
#xl  <- "(age+age2 +male +urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode)^2" #use this for rlasso etc.

x <- "age"
xl <- "(age)^2"
# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble
Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.02, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Boosting", "Forest", "Nnet"))              # methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest", "Boosting", "Nnet", "Ensemble")        # ML methods that are used in estimation
split        <- 1            #number of splits                                                      # number of splits


################################ Estimation ##################################################

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
# ensemble  : ML methods used ine ensemble method
# silent    : whether to print messages
# trim      : bounds for propensity score trimming


r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data1, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

################################ Compute Output Table ########################################

result           <- matrix(0,3, length(methods)+1)
colnames(result) <- cbind(t(methods), "best")
rownames(result) <- cbind("Median ATE", "se(median)",  "se")

result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result, digits = 3)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))










########################################################################

#construct Histograms

#part2:

#construct tests


#part3:

#construct test statistics

#part4: (ALEX)

