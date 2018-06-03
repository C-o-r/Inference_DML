# Table 1
# Replica + DML
#
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

#1. Replica in R

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

# make Dx for each x regression in Table1
Y <- df$trust_neighbors
D=df$exports

D1= df$exports

D2= df$export_area

D3= df$export_pop

D4= df$ln_exports

D5= df$ln_export_area

D6= df$ln_export_pop

#@ignore
#baline_controls=df[,c(1,2,5)]
#baline_controls=df[ , c("age","age2","male","urban_dum","district_ethnic_frac","frac_ethnicity_in_district")]
#baline_controls_plus_variables_not_in_dummyform = df[ , c("education","occupation","religion","living_conditions","isocode")]

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




#"age age2 male urban_dum i.education i.occupation i.religion i.living_conditions district_ethnic_frac frac_ethnicity_in_district i.isocode"

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


#@ignore
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district, data=df)
#summary(test_regression) 
# -0.00061982368 parameter deviates from ???0.00068
# try simplty with those regressors; FAIL
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 
# -0.00076440340 parameter deviates from ???0.00068
# works a bit; close to D 
#test_regression <- lm( Y~ D+ age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education+occupation+religion+living_conditions+isocode, data=df)
#summary(test_regression) 
# -0.00076440340 parameter deviates from ???0.00068 exlucing several dummies


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


# final Table 1 with correct estimated coefficients and R^2
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

colnames(result) <- cbind("Slasve exports (thousands)","Exports/area","Exports/historical pop","ln(1+exports)","ln(1+exports/area)","ln(1+exports/historical pop")
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


# create dataset for DML Algorithm
#data1 <- data.frame(Y,D1,D2,D3,D4,D5,D6,age,age2,male,urban_dum,district_ethnic_frac,education_fac,occupation_fac,religion_fac,living_conditions_fac,isocode)
data1 <-data.frame(Y,D1,age,age2)
data1_na <- na.omit(data1) # 20325 from 21822

data1_na_short <-  data1_na[1:1000,]

#data1= data1[1:50,]
#write.xlsx(data1_1, "c:/Users/cor64/Documents/data1_1.xlsx")

#write.dta(data1, "C:/data1.dta")

#write.xls(data1)

################ Loading functions and Data ########################


#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1210);
cl <- makeCluster(12, outfile="")



# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D1";  

# Controls_ALL
#x      <- "age+age2+male+urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode"         # use this for tree-based methods like forests and boosted trees
#xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode)^2";     # use this for rlasso etc.

# increase Controls iteratively to check wether it works

# Controls_1
x      <- "age+age2"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2)^2";     # use this for rlasso etc.

# Controls_1; works
data1 <-data.frame(Y,D1,age,age2)
data1_na <- na.omit(data1) # 20325 from 21822
data1_na_short <-  data1_na[1:1000,]

# Controls_2; works
data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac)
data1_na <- na.omit(data1) # 
data1_na_short <-  data1_na[1:1000,]

# Controls_3; doesnt work 
data1 <-data.frame(Y,D1,education_fac,occupation_fac,religion_fac,living_conditions_fac,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]

# Controls_4; 
data1 <-data.frame(Y,D1,isocode,age)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]

# Controls_5; doesnt work yet 

# Controls_6; 
data1 <-data.frame(Y,D1,religion,age)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]


# Controls_7; doesnt work 
data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]


# Controls_8; 
data1 <-data.frame(Y,D1,occupation,religion,education,living_conditions)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]


# Controls_9; 
data1 <-data.frame(Y,D1,occupation_fac,religion_fac)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]

# Controls_10
data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na[1:1200,]


# Controls_1; works
#x      <- "age+age2"         # use this for tree-based methods like forests and boosted trees
#xl     <- "(age+age2)^2";     # use this for rlasso etc.


# Controls_2; works
x      <- "age+age2+male+urban_dum+district_ethnic_frac"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac)^2";     # use this for rlasso etc.


# Controls_3
x      <- "education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode"         # use this for tree-based methods like forests and boosted trees
xl     <- "(education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode)^2";     # use this for rlasso etc.

# Controls_4
x      <- "isocode+age"         # use this for tree-based methods like forests and boosted trees
xl     <- "(isocode+age)^2";     # use this for rlasso etc.

# Controls_5
#x      <- "age+age2+male+urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode"         # use this for tree-based methods like forests and boosted trees
#xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode)^2";     # use this for rlasso etc.


#religion,age: levels dont work though ??? why ???
# Controls_6
x      <- "religion+age"         # use this for tree-based methods like forests and boosted trees
xl     <- "(religion+age)^2";     # use this for rlasso etc.


#age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode


# Controls_7; passes without boosting
x      <- "age+age2+male+urban_dum+district_ethnic_frac+occupation+religion+education+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+occupation+religion+education+living_conditions+isocode)^2";     # use this for rlasso etc.



# Controls_8; works
x      <- "occupation+religion+education+living_conditions"         # use this for tree-based methods like forests and boosted trees
xl     <- "(occupation+religion+education+living_conditions)^2";     # use this for rlasso etc.


# Controls_9; works
x      <- "occupation_fac+religion_fac"         # use this for tree-based methods like forests and boosted trees
xl     <- "(occupation_fac+religion_fac)^2";     # use this for rlasso etc.


# Controls_10; workds
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.


#######################################################################################################################
#Full computation Table 1 so far invloving all controls; 

# 1. D1
data1 <-data.frame(Y,D1,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data1_na <- na.omit(data1) 
data1_na_short <-  data1_na
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

#Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data1_na_short, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

result1           <- matrix(0,3, length(methods)+1)
colnames(result1) <- cbind(t(methods), "best")
rownames(result1) <- cbind("Median ATE", "se(median)",  "se")

result1[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result1[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result1[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

############################################################################################################################


# 2. D2
data2 <-data.frame(Y,D2,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data2_na <- na.omit(data2) 
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D2";  
data2_na_short <-  data2_na

x2      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl2     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

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

# 3. D3
data3 <-data.frame(Y,D3,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data3_na <- na.omit(data3) 
data3_na_short <-  data3_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D3";  
x3      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl3    <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.


Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

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

# 4. D4
data4 <-data.frame(Y,D4,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data4_na <- na.omit(data4) 
data4_na_short <-  data4_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D4";  
x4      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl4     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

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

# 5. D5
data5 <-data.frame(Y,D5,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data5_na <- na.omit(data5) 
data5_na_short <-  data5_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D5";  
x5     <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl5     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.


Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

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


# 6. D6
data6 <-data.frame(Y,D6,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode)
data6_na <- na.omit(data6) 
data6_na_short <-  data6_na
# Outcome Variable
y      <- "Y";

# Treatment Indicator
d      <- "D6";  
x6      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode"         # use this for tree-based methods like forests and boosted trees
xl6     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode)^2";     # use this for rlasso etc.

Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits

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

#Boosting     <- list(bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
Nnet         <- list(size=8,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1                                                               # number of splits



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
# ensemble  : ML methods used in ensemble method
# silent    : whether to print messages
# trim      : bounds for propensity score trimming


r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data1_na_short, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}

################################ Compute Output Table ########################################

result           <- matrix(0,3, length(methods)+1)
colnames(result) <- cbind(t(methods), "best")
rownames(result) <- cbind("Median ATE", "se(median)",  "se")

result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

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


###########################################################################
###########################################################################








