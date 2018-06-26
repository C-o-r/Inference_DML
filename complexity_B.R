# complexity covariates:
#386879
data(AJR)
setwd("C:\\Users\\cor64\\Desktop\\Uni\\Erasmus\\Thesis\\DMLonGitHub-master\\")

library(ivmodel)
library(AER)
require(ggplot2)
require(plotrix)
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
library(nnet)
library(plotrix)
library(RColorBrewer)



#1.IV

y  <- "GDP" #:A? log? 
d  <- "Exprop"
z  <- "logMort"	
x1  <- "Latitude"
x2  <- "Latitude + Latitude2 "
x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.
xla <- "(Latitude)^2+(Latitude2)^2+(Africa)^2+(Asia)^2+(Namer)^2+(Samer)^2" 


y  <- "GDP" #:A? log? 
d  <- "Exprop"
z  <- "logMort"	

amount_cov = seq(0,6)
Par = rep(0,10)
uc = rep(0,10)
lc = rep(0,10)

#update formulae



for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude)^2+(Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude)^2+(Latitude2)^2+(Africa)^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa +  Asia"
  xla <- "(Latitude)^2+(Latitude2)^2+(Africa)^2+(Asia)^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude)^2+(Latitude2)^2+(Africa)^2+(Asia)^2+(Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude)^2+(Latitude2)^2+(Africa)^2+(Asia)^2+(Namer)^2"}
  
  
  

form            <- as.formula(paste(y, "~", d));
form2            <- as.formula(paste(y, "~", d, "|",z));
form_final1      <-as.formula(paste(y, "~", d , "|",z));
form_ff      <-as.formula(paste(y, "~", d ,"+", x ,"+", xla,"|",z,"+", x,"+",xla));
###

#regressions
#fit           <- lm(form,  x = TRUE, y = TRUE, data=AJR);
#fit           <- lm(form, data=AJR);
#fm1 <- ivreg(form,data=AJR)
#fm2 <- ivreg(AJR$GDP~AJR$Exprop|AJR$logMort,data=AJR)
#fm3 <- ivreg(AJR$GDP~AJR$Exprop|AJR$logMort,data=AJR)
#fm3 <- ivreg(form2,data=AJR)
fmfd <- ivreg(form_ff  ,data=AJR)
#fmfd <- ivreg(form_ff2  ,data=AJR)



Par[i] = fmfd$coefficients[2]
uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]

}



results=data.frame(Par,uc,lc)
results = results[2:6,]

       

#####NNET

#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

  ############################### Inputs ########################################
  
  # Outcome Variable
  y  <- "GDP" #:A? log? 
  
  # Treatment Variable
  d  <- "Exprop"
  
  # Instrument
  z  <- "logMort"	
  
  
  x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
  xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.
  
  
  par = seq(0,8)
  lcn = seq(0,8)
  ucn = seq(0,8)
  for (i in 2:6) {
    
    if (i == 1) {}
    else if (i==2) {
      
      x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
      xla <- "(Latitude + Latitude2)^2" 
    }
    else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2 + Africa )^2 "} 
    else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
    xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
    else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
    xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
    else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
    xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
    
    
  
  
  # Controls
 # x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
  #xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.
  
  

  
  
  split        <- 2  
  
  
  Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=TRUE)
  methods      <- c("Nnet")         # method names to be estimated
  arguments    <- list(Nnet=Nnet)
  
  
  
  
  

  

  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  par[i] = result[1]
  lcn[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucn[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
  }
  


  

resultsfinalNNET = par[2:6]
lcn = lcn[2:6]
ucn = ucn[2:6]

  
#######################################################################
#Trees


#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

############################### Inputs ########################################

# Outcome Variable
y  <- "GDP" #:A? log? 

# Treatment Variable
d  <- "Exprop"

# Instrument
z  <- "logMort"	


x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.


par1 = seq(0,8)
lcn1 = seq(0,8)
ucn1 = seq(0,8)
for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude + Latitude2 + Africa )^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
  xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
  
  

  
  split        <- 2  
  
  
  Trees        <- list(reg_method="anova", clas_method="class")
  methods      <- c("Trees")         # method names to be estimated
  arguments    <- list( Trees=Trees)
  
  

  
  
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  par1[i] = result[1]
  lcn1[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucn1[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
}

resultst = par1[2:6]
lcnt = lcn1[2:6]
ucnt = ucn1[2:6]


  
  
write.csv(y, file = "c2_Nnet_AJR_200.csv", row.names = FALSE)






#######################################################################
#Forrest


#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

############################### Inputs ########################################

# Outcome Variable
y  <- "GDP" #:A? log? 

# Treatment Variable
d  <- "Exprop"

# Instrument
z  <- "logMort"	


x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.


par2 = seq(0,8)
lcn2 = seq(0,8)
ucn2 = seq(0,8)
for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude + Latitude2 + Africa )^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
  xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
  
  
  
  
  split        <- 2  
  
  
  Forest       <- list(clas_nodesize=1, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
  methods      <- c("Forest")         # method names to be estimated
  arguments    <- list(Forest=Forest)
  
  
  
  
  
  
  
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  par2[i] = result[1]
  lcn2[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucn2[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
}





resultsfinalfor = par2[2:6]
lcnt2 = lcn2[2:6]
ucnt2 = ucn2[2:6]


############################################################################
#Lasso



#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

############################### Inputs ########################################

# Outcome Variable
y  <- "GDP" #:A? log? 

# Treatment Variable
d  <- "Exprop"

# Instrument
z  <- "logMort"	


x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.


parll = seq(0,8)
lcnll = seq(0,8)
ucnll = seq(0,8)
for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude + Latitude2 + Africa )^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
  xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
  
  
  
  
  split        <- 2  
  
  
  RLasso       <- list(penalty = list(homoscedastic = FALSE, X.dependent.lambda =FALSE, lambda.start = NULL, c = 1.1), intercept = TRUE)
  methods      <- c("RLasso")         # method names to be estimated
  arguments    <- list( RLasso=RLasso)
  
  
  
  
  
  
  
  
  
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  parll[i] = result[1]
  lcnll[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucnll[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
}





resultsfinall = parll[2:6]
lcnll = lcnll[2:6]
ucnll = ucnll[2:6]



####################################


# 
# lines(x,resultsfinall , type="b", pch=24, col="black", lty=2)
# lines(x,lcntl , type="b", pch=24, col="green", lty=2)
# lines(x,ucntl , type="b", pch=24, col="green", lty=2)
# 
##########################################

#Boosting




#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

############################### Inputs ########################################

# Outcome Variable
y  <- "GDP" #:A? log? 

# Treatment Variable
d  <- "Exprop"

# Instrument
z  <- "logMort"	


x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.


parb = seq(0,8)
lcb = seq(0,8)
ucb = seq(0,8)
for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude + Latitude2 + Africa )^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
  xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
  
  
  
  
  split        <- 2  
  
  
  Boosting     <- list(bag.fraction = 1, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=5, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
  methods      <- c("Boosting")         # method names to be estimated
  arguments    <- list( Boosting=Boosting)
  
  
  
  
  
  
  
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  parb[i] = result[1]
  lcb[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucb[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
}





resultsb = parb[2:6]
lcb = lcb[2:6]
ucb = ucb[2:6]










#################################################################
#@ hybrid 




#rm(list = ls())  # Clear everything out so we're starting clean

source("ML_Functions.R")  
source("Moment_Functions.R")  
options(warn=-1)
set.seed(1211);
cl   <- makeCluster(2, outfile="")

data(AJR)
#######

data=AJR

############################### Inputs ########################################

# Outcome Variable
y  <- "GDP" #:A? log? 

# Treatment Variable
d  <- "Exprop"

# Instrument
z  <- "logMort"	


x  <- "Latitude + Latitude2 + Africa +  Asia + Namer + Samer"       # use this for tree-based methods like forests and boosted trees
xl <- "(Latitude + Latitude2 + Africa +  Asia + Namer + Samer)^2"   # use this for rlasso etc.


parhy = seq(0,8)
lchy = seq(0,8)
uchy = seq(0,8)
for (i in 2:6) {
  
  if (i == 1) {}
  else if (i==2) {
    
    x  <- "Latitude + Latitude2 "       # use this for tree-based methods like forests and boosted trees
    xla <- "(Latitude + Latitude2)^2" 
  }
  else if (i==3) {      x  <- "Latitude + Latitude2 +Africa"       # use this for tree-based methods like forests and boosted trees
  xla <- "(Latitude + Latitude2 + Africa )^2 "} 
  else if (i==4) {x  <- "Latitude + Latitude2 +Africa"
  xla <- "(Latitude + Latitude2 + Africa +  Asia )^2" }
  else if (i==5) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer  "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer)^2" }
  else if (i==6) {x  <- "Latitude + Latitude2 +Africa +  Asia + Namer + Samer "
  xla <- "(Latitude + Latitude2 + Africa +  Asia + Namer+Samer)^2"}
  
  
  
  
  # Method names: Boosting, Nnet, RLasso, PostRLasso, Forest, Trees, Ridge, Lasso, Elnet, Ensemble
  
  Boosting     <- list(n.minobsinnode = 1, bag.fraction = .5, train.fraction = 1.0, interaction.depth=2, n.trees=1000, shrinkage=.01, n.cores=1, cv.folds=2, verbose = FALSE, clas_dist= 'adaboost', reg_dist='gaussian')
  Forest       <- list(clas_nodesize=3, reg_nodesize=5, ntree=1000, na.action=na.omit, replace=TRUE)
  RLasso       <- list(intercept = TRUE)
  Nnet         <- list(size=2,  maxit=1000, decay=0.01, MaxNWts=10000,  trace=FALSE)
  Trees        <- list(reg_method="anova", clas_method="class")
  
  arguments    <- list(Boosting=Boosting, Forest=Forest, RLasso=RLasso, Nnet=Nnet, Trees=Trees)
  
  ensemble     <- list(methods=c("RLasso","Trees", "Boosting", "Forest", "Nnet"))                       # methods for the ensemble estimation
  methods      <- c("Ensemble")          # ML methods that are used in estimation
  #methods      <- c("RLasso","RLasso")          # ML methods that are used in estimation
  
  split        <- 2# number of splits
  
  
 
  
  
  
  r <- foreach(k = 1:split, .combine='rbind', .inorder=FALSE, .packages=c('MASS','randomForest','neuralnet','gbm', 'sandwich', 'hdm', 'nnet', 'rpart','glmnet')) %dopar% { 
    
    dml <- DoubleML(data=data, y=y, d=d, z=NULL, xx=x, xL=xl, methods=methods, DML="DML2", nfold=2, est="plinear", arguments=arguments, ensemble=ensemble, silent=FALSE, trim=c(0.01,0.99)) 
    
    data.frame(t(dml[1,]), t(dml[2,]))
    
  }
  
  
  result           <- matrix(0,3, length(methods)+1)
  colnames(result) <- cbind(t(methods), "best")
  rownames(result) <- cbind("Median ATE", "se(median)",  "se")
  result[1,]        <- colQuantiles(r[,1:(length(methods)+1)], probs=0.5)
  result[2,]        <- colQuantiles(sqrt(r[,(length(methods)+2):ncol(r)]^2+(r[,1:(length(methods)+1)] - colQuantiles(r[,1:(length(methods)+1)], probs=0.5))^2), probs=0.5)
  result[3,]        <- colQuantiles(r[,(length(methods)+2):ncol(r)], probs=0.5)
  
  parb[i] = result[1]
  lcb[i] = result[1]-(1.96*result[3]/sqrt(64))
  ucb[i]=result[1]+(1.96*result[3]/sqrt(64))
  #uc[i] = confint(fmfd, 'Exprop', level=0.95)[1]
  #lc[i] = confint(fmfd, 'Exprop', level=0.95)[2]
  
}





resulthybrid = parb[2:6]
lchy = lcb[2:6]
uchy = lcb[2:6]




#make picture 


x <- c(2:6)

opar <- par(no.readonly=TRUE)


par(bg = "white") #check
plot(x, results$Par, type="b",
     pch=1, col="red",
     yaxt="n", lty=3,ylim=c(0.32, 0.9), ann=FALSE)
#lines(x,results$uc , type="b", pch=23, col="red", lty=2)
#lines(x,results$lc , type="b", pch=23, col="red", lty=2)


lines(x,resultsfinalNNET , type="b", pch=25, col="blue", lty=2)
#lines(x,lcn , type="b", pch=25, col="blue", lty=2)
#lines(x,ucn , type="b", pch=25, col="blue", lty=2)

#check

lines(x,resultst , type="b", pch=23, col="brown", lty=2)
#lines(x,lcnt , type="b", pch=23, col="brown", lty=2)
#lines(x,ucnt , type="b", pch=23, col="brown", lty=2)


lines(x,resultsfinalfor , type="b", pch=23, col="green", lty=2)
#lines(x,lcnt2 , type="b", pch=23, col="green", lty=2)
#lines(x,ucnt2 , type="b", pch=23, col="green", lty=2)

lines(x,resultsfinall , type="b", pch=24, col="black", lty=2)
#lines(x,lcnll  , type="b", pch=24, col="black", lty=2)
#lines(x,ucnll , type="b", pch=24, col="black", lty=2)




lines(x,
      resultsb
      , type="b", pch=24, col="pink", lty=2)
#lines(x,lcb  , ty0pe="b", pch=24, col="pink", lty=2)
#lines(x,ucb , type="b", pch=24, col="pink", lty=2)


lines(x,resulthybrid, type="b", pch=24, col="orange", lty=2)
#lines(x,lchy  , type="b", pch=24, col="orange", lty=2)
#lines(x,uchy , type="b", pch=24, col="orange", lty=2)

axis(2,  col.axis="black", las=2)

legend("bottom",bg="transparent", inset=.05, title="Algorithm type", c("IV","Nnet","Tree","Forrest","Lasso","Boosting","Hybrid"),
       lty=c(1, 7), pch=c(21,25,23,24,25,26), col=c("red","blue","brown","green","black","pink","orange"),box.lty=0)

#Algorithm estimators by interatively increasing covariates of ADJ
title("Estimations for parameter of interest",
      xlab="amount of covariates",
      ylab="values")


#summary(fm)
#summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)