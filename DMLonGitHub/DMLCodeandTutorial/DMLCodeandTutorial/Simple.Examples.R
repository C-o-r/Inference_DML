require(AER)
require(hdm)
require(RandomForest)


# Simple DML2 Code for Partrially Linear Model

DML2.for.PLM <- function(x, d, y, dreg, yreg, nfold=2) {
# this implements DML2 algorithm, where there moments are estimated via DML, before constructing
# the pooled estimate of theta randomly split data into folds
nobs <- nrow(x)
foldid <- rep.int(1:nfold,times = ceiling(nobs/nfold))[sample.int(nobs)]
I <- split(1:nobs, foldid)
# create residualized objects to fill
ytil <- dtil <- rep(NA, nobs)
# obtain cross-fitted residuals
cat("fold: ")
for(b in 1:length(I)){
  dfit <- dreg(x[-I[[b]],], d[-I[[b]]])  #take a fold out
  yfit <- yreg(x[-I[[b]],], y[-I[[b]]])  # take a folot out
  dhat <- predict(dfit, x[I[[b]],], type="response")  #predict the fold out
  yhat <- predict(yfit, x[I[[b]],], type="response")  #predict the fold out
  dtil[I[[b]]] <- (d[I[[b]]] - dhat) #record residual
  ytil[I[[b]]] <- (y[I[[b]]] - yhat) #record residial
  cat(b," ")
}
  rfit <- lm(ytil ~ dtil)               #estimate the main parameter by regressing one residual on the other
  coef.est <- coef(rfit)[2]             #extract coefficient 
  se <- sqrt(vcovHC(rfit)[2,2])         #record standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se))
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil) )
}

# Simple DML1 Code for Partially Linear Model (DML2 is recommended)

DML1.for.PLM <- function(x, d, y, dreg, yreg, nfold=2) {
  # this implements DML1 algorithm, where there moments are estimated via DML, before constructing
  # the pooled estimate of theta  randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold, times = ceiling(nobs/nfold))[sample.int(nobs)] #fold IDs
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- rep(NA, nobs)
  coef.est <- rep(NA, length(I))
  # obtain cross-fitted residuals
  cat("fold: ")
  for(b in 1:length(I)){
  dfit <- dreg(x[-I[[b]],], d[-I[[b]]])
  yfit <- yreg(x[-I[[b]],], y[-I[[b]]])
  dhat <- predict(dfit, x[I[[b]],], type="response")
  yhat <- predict(yfit, x[I[[b]],], type="response")
  dtil[I[[b]]] <- (d[I[[b]]] - dhat)
  ytil[I[[b]]] <- (y[I[[b]]] - yhat)
  coef.est[b]<- mean(dtil[I[[b]]]*ytil[I[[b]]])/mean(dtil[I[[b]]]*dtil[I[[b]]])
  cat(b," ")}
  coef.est <- mean(coef.est)
  se <- sqrt(mean((ytil-coef.est*dtil)^2*dtil^2)/(mean(dtil^2)^2))/sqrt(length(dtil)-1)
  cat(sprintf("\ncoef (se) = %g (%g)\n",coef.est, se))
  return( list(coef.est=coef.est, se=se, dtil=dtil, ytil=ytil) )
}


#Example

# data from HDM package

library(hdm)
library(randomForest)
data(Growth)
str(GrowthData)

y= as.matrix(GrowthData[,1])          #outcome	
d= as.matrix(GrowthData[,3])          #treatment	
x= as.matrix(GrowthData[,-c(1,2,3)])  #controls

DS.lasso= summary(rlassoEffect(x, y,d))  #DoubleSelection with rlasso


#DML with Lasso


dreg <- function(x,d){ rlasso(x, d) }  #ML method= rlasso
yreg <- function(x,y){ rlasso(x, y) }  #ML method = rlasso

set.seed(1)
DML2.lasso = DML2.for.PLM(x, d, y, dreg, yreg, nfold=5)
set.seed(1)
DML1.lasso = DML1.for.PLM(x, d, y, dreg, yreg, nfold=5)


#DML with Random Forest

dreg <- function(x,d){ randomForest(x, d) }  #ML method=Forest
yreg <- function(x,y){ randomForest(x, y) }  #ML method=Forest

set.seed(1)
DML2.RF = DML2.for.PLM(x, d, y, dreg, yreg, nfold=5)
set.seed(1)
DML1.RF = DML1.for.PLM(x, d, y, dreg, yreg, nfold=5)

# Compare Forest vs Lasso

comp.tab= matrix(NA, 2, 2)
comp.tab[1,] = c( sqrt(var(DML2.RF$ytil)),  sqrt(var(DML2.lasso$ytil)))
comp.tab[2,] = c( sqrt(var(DML2.RF$dtil)),  sqrt(var(DML2.lasso$dtil)))
rownames(comp.tab) = c("RMSE for Y:", "RMSE for D:")
colnames(comp.tab) = c("RF", "LASSO")
print(comp.tab, digits=3)

# Conclude that Forest is (slighly) better
 
 
##################################

# DML for PLIVM

DML2.for.PLIVM <- function(x, d, z, y, dreg, yreg, zreg, nfold=2) {
  # this implements DML2 algorithm, where there moments are estimated via DML, before constructing
  # the pooled estimate of theta randomly split data into folds
  nobs <- nrow(x)
  foldid <- rep.int(1:nfold,times = ceiling(nobs/nfold))[sample.int(nobs)]
  I <- split(1:nobs, foldid)
  # create residualized objects to fill
  ytil <- dtil <- ztil<- rep(NA, nobs)
  # obtain cross-fitted residuals
  cat("fold: ")
  for(b in 1:length(I)){
    dfit <- dreg(x[-I[[b]],], d[-I[[b]]])  #take a fold out
    zfit <- zreg(x[-I[[b]],], z[-I[[b]]])  #take a fold out
    yfit <- yreg(x[-I[[b]],], y[-I[[b]]])  # take a folot out
    dhat <- predict(dfit, x[I[[b]],], type="response")  #predict the fold out
    zhat <- predict(zfit, x[I[[b]],], type="response")  #predict the fold out
    yhat <- predict(yfit, x[I[[b]],], type="response")  #predict the fold out
    dtil[I[[b]]] <- (d[I[[b]]] - dhat) #record residual
    ztil[I[[b]]] <- (z[I[[b]]] - zhat) #record residual
    ytil[I[[b]]] <- (y[I[[b]]] - yhat) #record residial
    cat(b," ")
  }
  ivfit= tsls(y=ytil,d=dtil, x=NULL, z=ztil, intercept=FALSE)
  coef.est <-  ivfit$coef          #extract coefficient 
  se <-  ivfit$se                  #record standard error
  cat(sprintf("\ncoef (se) = %g (%g)\n", coef.est , se))
  return( list(coef.est =coef.est , se=se, dtil=dtil, ytil=ytil, ztil=ztil) )
}

data(AJR); 
y = AJR$GDP; 
d = AJR$Exprop; 
z = AJR$logMort
x = model.matrix(~ -1 + (Latitude + Latitude2 + Africa + 
                           Asia + Namer + Samer)^2, data=AJR)
dim(x)

# DML with Random Forest

dreg <- function(x,d){ randomForest(x, d) }  #ML method=Forest
yreg <- function(x,y){ randomForest(x, y) }  #ML method=Forest
zreg<-  function(x,z){ randomForest(x, z)}  #ML method=Forest 
  
set.seed(1)
DML2.RF = DML2.for.PLIVM(x, d, z, y, dreg, yreg, zreg, nfold=20)

# DML with PostLasso


dreg <- function(x,d){ rlasso(x, d) }  #ML method=lasso
yreg <- function(x,y){ rlasso(x, y) }  #ML method=lasso
zreg<-  function(x,z){ rlasso(x, z)}  #ML method=lasso 

set.seed(1)
DML2.lasso = DML2.for.PLIVM(x, d, z, y, dreg, yreg, zreg, nfold=20)


# Compare Forest vs Lasso

comp.tab= matrix(NA, 3, 2)
comp.tab[1,] = c( sqrt(var(DML2.RF$ytil)),  sqrt(var(DML2.lasso$ytil)))
comp.tab[2,] = c( sqrt(var(DML2.RF$dtil)),  sqrt(var(DML2.lasso$dtil)))
comp.tab[3,] = c( sqrt(var(DML2.RF$ztil)),  sqrt(var(DML2.lasso$ztil)))
rownames(comp.tab) = c("RMSE for Y:", "RMSE for D:", "RMSE for Z:")
colnames(comp.tab) = c("RF", "LASSO")
print(comp.tab, digits=3)

