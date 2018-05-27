# Econometrics Seminar-2018
# 27.05.18
Sys.info()  
options(scipen=999)
#install.packages(dummies)
#library(dummies)

#library(benchmarkme)
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
#library(dummies)

setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")

#read Data
df = read.xls ("Slave.xls", sheet = 1, header = TRUE)

#source("Monte_Carlo.R")
#check works


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
D1= df$exports

D2= df$export_area

D3= df$export_pop

D4= df$ln_exports

D5= df$ln_export_area

D6= df$ln_export_pop

#baline_controls=df[,c(1,2,5)]
baline_controls=df[ , c("age","age2","male","urban_dum","district_ethnic_frac","frac_ethnicity_in_district")]

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
baline_controls_plus_variables_not_in_dummyform
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 

# -0.00076440340 parameter deviates from ???0.00068



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


#dummy <- as.numeric(education == 1957)

baline_controls_plus_variables_not_in_dummyform
test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions+isocode, data=df)
summary(test_regression) 

# -0.00076440340 parameter deviates from ???0.00068

#create a dummy for each value of education occupation religion living_conditions and isode_code

educ_0 <- as.numeric(education == 0)
educ_1 <- as.numeric(education == 1)
educ_2 <- as.numeric(education == 2)
educ_3 <- as.numeric(education == 3)
educ_4 <- as.numeric(education == 4)
educ_5 <- as.numeric(education == 5)
educ_6 <- as.numeric(education == 6)
educ_7 <- as.numeric(education == 7)
educ_8 <- as.numeric(education == 8)
educ_9 <- as.numeric(education == 9)

#educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9

test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+educ_0 +educ_1 +educ_2 +educ_3 +educ_4 +educ_5 +educ_6 +educ_7 +educ_8 +educ_9 +occupation+religion+living_conditions+isocode, data=df)
summary(test_regression) 


# works a bit; close to D 
test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
summary(test_regression) 


#new trial with factored stuff 
#Table 1_1
test_regression <- lm( Y~ D1+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 
# works okayish ~-0.00067914 vs. -0.00068
# R^2 = 0.16 vs. 0.1558 (unadjusted, adjusted even smaller 0.1526 (( )))
# std error stil false 0.00004855 vs ~ 0.0013


#Table 1_2

test_regression <- lm( Y~ D2+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 

#Table 1_3
test_regression <- lm( Y~ D3+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 

#Table 1_4
test_regression <- lm( Y~ D4+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 

#Table 1_5
test_regression <- lm( Y~ D5+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 

#Table 1_6
test_regression <- lm( Y~ D6+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode, data=df)
summary(test_regression) 


###################### Loading functions and Data ##############################

rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


data_input=


#data(AJR); 


################################ Inputs ########################################

# Outcome Variable
#y  <- "GDP" #:A? log? 

y <- "trust_neighbors"
# Treatment Variable
d  <- "exports"


# Controls

x <- "age +age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode" # use this for tree-based methods like forests and boosted trees
xl  <- "(age +age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode)^2" #use this for rlasso etc.

# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble
Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Boosting", "Forest"))                       # methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Boosting", "Forest", "Nnet","Ensemble")          # ML methods that are used in estimation
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation

split        <- 3                                                                  # number of splits

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
  
  dml <- DoubleML(data=df, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
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

