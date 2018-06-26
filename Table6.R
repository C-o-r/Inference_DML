# Table 6
# Replica + DML

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


#1. Replica in R




# prepare variables 

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
Y_TN <- df$trust_neighbors
Y_TR <- df$trust_relatives
Y_TC <- df$trust_local_council
Y_tra <- df$intra_group_trust
Y_ter <- df$inter_group_trust
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


# colonial controls

mal = df$malaria_ecology
mis=df$total_missions_area
ex_con=factor(df$explorer_contact)
rail=factor(df$railway_contact)
c1400_=factor(df$cities_1400_dum)
v30_fac= factor(df[ , c("v30")])
v33=factor(df$v33)


#anther control
intit_pop=df$ln_init_pop_density
intit_pop[is.na(intit_pop)] <- 1 #For instance

#distance
dist= df$distsea

#fishing
#dist_Saharan_node
#dist_Saharan_line if missing(`x')~=1,

fish=df$fishing
Sah_N=df$dist_Saharan_node
Sah_l=df$dist_Saharan_line
Sah_l[is.na(Sah_l)] <- 1 #For instance





#Table 6_1 Trust of relatives 
#ivreg(Y ~ X + W | W + Z, ... )
test_regression1 = ivreg( Y_TR~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression1)
summary(test_regression1, diagnostics=TRUE)
#  -0.170 vs -0.172; close; correct R-squared for 2nd stage

#@ignore
# Hausman
#first_stage <- lm(D5~dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop,data=df)
# Hausman
#Hausman_reg <- lm(Y_TR~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+first_stage$residuals,data=df)
#print(summary(Hausman_reg))
#summary(test_regression1, vcov = sandwich, diagnostics = TRUE)



#Table 5_2 Trust of neighbors
#ivreg(Y ~ X + W | W + Z, ... )
test_regression2 = ivreg( Y_TN~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression2)
#  -0.197 vs -0.271; correct r-squared 


#Table 5_3 trust of council
test_regression3 = ivreg( Y_TC~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression3)
#...

#Table 5_4 turst intra
test_regression4 = ivreg( Y_tra~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression4)
#...


#Table 5_5 trust inter
test_regression5 = ivreg( Y_ter~ D5+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l
                          | dist+age+age2 +male +urban_dum+district_ethnic_frac+frac_ethnicity_in_district+education_fac+occupation_fac+religion_fac+living_conditions_fac+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop1+fish+Sah_N+Sah_l,data = df)
summary(test_regression5)

# final Table 1 with correct estimated coefficients and R^2
result <- matrix(0,3, 5)
result[1,1]=round(summary(test_regression1)$coefficients[2, 1], digits =5)
result[1,2]=round(summary(test_regression2)$coefficients[2, 1] , digits =3)
result[1,3]=round(summary(test_regression3)$coefficients[2, 1] , digits =3)
result[1,4]=round(summary(test_regression4)$coefficients[2, 1], digits =3)
result[1,5]=round(summary(test_regression5)$coefficients[2, 1], digits =3)

result[2,1]=round(summary(test_regression1)$r.squared, digits =2)
result[2,2]=round(summary(test_regression2)$r.squared , digits =2)
result[2,3]=round(summary(test_regression3)$r.squared , digits =2)
result[2,4]=round(summary(test_regression4)$r.squared , digits =2)
result[2,5]=round(summary(test_regression5)$r.squared , digits =2)


colnames(result) <- cbind("Trust of relatives","Trust of neighbors","Trust of local council","Intragroup trust","Intergroup council")
rownames(result) <- cbind(" estimated IV coefficient for D5", "R-squared", "DML coefficient")

result_table <- round(result, digits = 5)


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

###results differ slightly, why ???

####
#DML

#Table6_1
###################### Loading functions and Data ##############################

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


################################ Inputs ########################################
data1 <-data.frame(Y_TR,D5,dist,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode,mal,mis,ex_con,rail,c1400_,v30_fac,v33,intit_pop,fish,Sah_N,Sah_l)

data1_na <- na.omit(data1) 
data1_na_short <-  data1_na


# Outcome Variable
y      <- "Y_TR";

# Treatment Indicator
d      <- "D5";  

# Instrument
z  <- "dist"	

# Controls
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l)^2";     # use this for rlasso etc.



# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

#Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1      
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation

split        <- 1# number of splits



r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data1_na_short, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}


result1           <- matrix(0,3, length(methods)+1)
colnames(result1) <- cbind(t(methods), "best")
rownames(result1) <- cbind("DML IV estimation for trust relatives", "se(median)",  "se")

result1[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result1[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result1[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result1, digits = 2)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))

################################################################################################################
#################################################################################################################


#Table5_2 Trust of neighbors
###################### Loading functions and Data ##############################

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


################################ Inputs ########################################
data2 <-data.frame(Y_TN,D5,dist,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode,mal,mis,ex_con,rail,c1400_,v30_fac,v33,intit_pop,fish,Sah_N,Sah_l)
data2_na <- na.omit(data2) 
data2_na_short <-  data2_na


# Outcome Variable
y      <- "Y_TN";

# Treatment Indicator
d      <- "D5";  

# Instrument
z  <- "dist"	

# Controls
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l)^2";     # use this for rlasso etc.



# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

#Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list( Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1      
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation

split        <- 1# number of splits



r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data2_na_short, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}


result2          <- matrix(0,3, length(methods)+1)
colnames(result2) <- cbind(t(methods), "best")
rownames(result2) <- cbind("DML IV estimation trust neighbors", "se(median)",  "se")

result2[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result2[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result2[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result2, digits = 2)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))

################################################################################################################
#################################################################################################################


#Table5_3 Trust council
###################### Loading functions and Data ##############################

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


################################ Inputs ########################################
data3 <-data.frame(Y_TC,D5,dist,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode,mal,mis,ex_con,rail,c1400_,v30_fac,v33,intit_pop,fish,Sah_N,Sah_l)
data3_na <- na.omit(data3) 
data3_na_short <-  data3_na


# Outcome Variable
y      <- "Y_TC";

# Treatment Indicator
d      <- "D5";  

# Instrument
z  <- "dist"	

# Controls
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l)^2";     # use this for rlasso etc.



# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1      
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation

split        <- 1# number of splits



r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data3_na_short, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}


result3         <- matrix(0,3, length(methods)+1)
colnames(result3) <- cbind(t(methods), "best")
rownames(result3) <- cbind("DML IV estimation for trust council", "se(median)",  "se")

result3[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result3[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result3[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result2, digits = 2)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))

################################################################################################################



#Table5_4; intra trust
###################### Loading functions and Data ##############################

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


################################ Inputs ########################################
data4 <-data.frame(Y_tra,D5,dist,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode,mal,mis,ex_con,rail,c1400_,v30_fac,v33,intit_pop,fish,Sah_N,Sah_l)
data4_na <- na.omit(data4) 
data4_na_short <-  data4_na


# Outcome Variable
y      <- "Y_tra";

# Treatment Indicator
d      <- "D5";  

# Instrument
z  <- "dist"	

# Controls
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l)^2";     # use this for rlasso etc.



# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1      
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation





r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data4_na_short, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}


result4         <- matrix(0,3, length(methods)+1)
colnames(result4) <- cbind(t(methods), "best")
rownames(result4) <- cbind("DML IV estimation for trust intra", "se(median)",  "se")

result4[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result4[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result4[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result4, digits = 5)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))

################################################################################################################


#Table5_5; inter trust
###################### Loading functions and Data ##############################

#rm(list = ls())  # Clear everything out so we're starting clean
source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")


################################ Inputs ########################################
data5 <-data.frame(Y_ter,D5,dist,age,age2,male,urban_dum,district_ethnic_frac, occupation, religion, education,living_conditions,isocode,mal,mis,ex_con,rail,c1400_,v30_fac,v33,intit_pop,fish,Sah_N,Sah_l)
data5_na <- na.omit(data5) 
data5_na_short <-  data5_na


# Outcome Variable
y      <- "Y_ter";

# Treatment Indicator
d      <- "D5";  

# Instrument
z  <- "dist"	

# Controls
x      <- "age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l"         # use this for tree-based methods like forests and boosted trees
xl     <- "(age+age2+male+urban_dum+district_ethnic_frac+education+occupation+religion+living_conditions+isocode+mal+mis+ex_con+rail+c1400_+v30_fac+v33+intit_pop+fish+Sah_N+Sah_l)^2";     # use this for rlasso etc.



# Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble

Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
RLasso       <- list(intercept = TRUE)
Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
Trees        <- list(reg_method="anova", clas_method="class")

arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)

ensemble     <- list(methods=c("RLasso", "Forest", "Nnet"))              # specify methods for the ensemble estimation
methods      <- c("RLasso","Trees", "Forest","Nnet", "Ensemble")         # method names to be estimated
split        <- 1      
#methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation





r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
  
  dml <- DoubleML(data=data5_na_short, y=y, d=d, z=z, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="IV", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=NULL) 
  
  data.frame(t(dml[1,]), t(dml[2,]))
  
}


result5        <- matrix(0,3, length(methods)+1)
colnames(result5) <- cbind(t(methods), "best")
rownames(result5) <- cbind("DML IV estimation for inter trust", "se(median)",  "se")

result5[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
result5[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
result5[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)

result_table <- round(result5, digits = 5)

for(i in 1:ncol(result_table)){
  for(j in seq(2,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
  for(j in seq(3,nrow(result_table),3)){
    
    result_table[j,i] <- paste("(", result_table[j,i], ")", sep="")
    
  }
}

print(xtable(result_table, digits=3))
#######################################################################
############################################################################################################