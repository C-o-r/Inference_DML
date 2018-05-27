---
title: "Simple Tutorial on DML for Partially Linear Models"
author: "VC"
date: "5/3/2018"
output: 
  slidy_presentation: 
    keep_md: yes
header-includes: 
  - \usepackage{tikz}
---



# DML in Partially Linear Model

\begin{eqnarray}\label{eq: PL1}
 &  Y = D\theta_0 + g_0(X) + \zeta,  &  E[\zeta \mid D,X]= 0,\\
  & D = m_0(X) +  V,   &  E[V \mid X] = 0, \label{eq: PL3}
\end{eqnarray}

where $Y$ is the outcome variable and $D$ is the policy variable of interest. The high-dimensional vector $$X = (X_1,..., X_{p})$$ consists of other confounding covariates, and $\zeta$ and $V$ are stochastic errors. The first equation is the equation of interest, and $\theta_0$ is the main regression coefficient that we would like to infer.   If $D$ is conditionally exogenous (randomly assigned conditional on $X$), $\theta_0$ has the interpretation of a structural or causal parameter.  The second equation keeps track of confounding, namely the dependence of $D$ on covariates/controls.  The characteristics $X$ affect the policy variable $D$ via the function $m_0(X)$ and the outcome variable via the function $g_0(X)$.   The partially linear model generalizes both linear regression models, where functions $g_0$
and $m_0$ are linear with respect to a dictionary of basis functions
with respect to $X$, and approximately linear models.   

-------

## Residualized Form

The PLR model above can be rewritten in the following residualized form:
\begin{eqnarray}\label{eq: PL}
 &&  W = V \theta_0 + \zeta,   \quad  E[\zeta \mid D,X]= 0,\\
  && W = (Y- \ell_0(X)),  \quad \ell_0(X) = E [Y \mid X], \\
  && V= (D - m_0(X)), \quad m_0(X) = E[D \mid X].
  \end{eqnarray}
  The variables above represent original variables after taking out or ``partialling out"
  the effect of $X$. Note that $\theta_0$ is identified from this equation if $V$ has
a non-zero variance. 


-------

# DML's Principle

Given identification, DML  proceeds
as follows

  1.  Estimate $\ell_0$ and $m_0$ by $\hat{\ell}_0$ and $\hat{m}_0$, which amounts to
solving the two problems of predicting $Y$ and $D$ using $X$, using any generic 
ML method, giving us estimated residuals 

$$\hat{W}= Y - \hat{\ell}_0(X) \text{ and } \hat{V} = D - \hat{m}_0(X).$$The estimates should be of a cross-fitted form, i.e. using sample spliting, as explained in the algorithm below. 

2.   Estimate $\theta_0$ by regressing the residual $\hat{W}$ on $\hat{V}$.  Use the conventional inference for this regression estimator, ignoring the estimation error in these residuals

The reason we work with this residualized form is that it eliminates the bias
arising when solving the prediction problems in stage 1.  The estimates $\hat \ell_0$ and $\hat m_0$ 
carry a regularization bias due to having to solve prediction problems well in high-dimensions.
However the nature of the estimating equation for $\theta_0$ are such that these biases are eliminated to the first order, as this is explained below.  The estimator is adaptive, in the sense that the first stage estimation errors do not affect the second  stage errors.

# DML1  for PLM


```r
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
```

# Simple DML2 Code for Partrially Linear Model


```r
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
```

----

# Example: Barro-Lee Growth Data


```r
library(AER)
library(hdm)
library(randomForest)
library(xtable)
data(Growth)
dim(GrowthData)
```

```
## [1] 90 63
```

```r
#str(GrowthData)
y= as.matrix(GrowthData[,1])          #outcome: growth rate
d= as.matrix(GrowthData[,3])          #treatment: initial wealth	
x= as.matrix(GrowthData[,-c(1,2,3)])  #controls: country characteristics
```

# Barro-Lee Data

* Y is a realized growth rate for wealth (per capita GPD) in a country
* D is an initial condition for wealth in the country
* X are characteristics of the country

# DML with Lasso


```r
dreg <- function(x,d){ rlasso(x, d) }  #ML method= rlasso
yreg <- function(x,y){ rlasso(x, y) }  #ML method = rlasso
set.seed(1)
DML2.lasso = DML2.for.PLM(x, d, y, dreg, yreg, nfold=5)
```

```
## fold: 1  2  3  4  5  
## coef (se) = -0.0409444 (0.0156979)
```

```r
set.seed(1)
DML1.lasso = DML1.for.PLM(x, d, y, dreg, yreg, nfold=5)
```

```
## fold: 1  2  3  4  5  
## coef (se) = -0.0354574 (0.015332)
```

```r
# compare to Double Selection
DS.lasso= summary(rlassoEffect(x, y,d))  #DoubleSelection with rlasso
```

#DML with Random Forest


```r
dreg <- function(x,d){ randomForest(x, d) }  #ML method=Forest
yreg <- function(x,y){ randomForest(x, y) }  #ML method=Forest
set.seed(1)
DML2.RF = DML2.for.PLM(x, d, y, dreg, yreg, nfold=5)
```

```
## fold: 1  2  3  4  5  
## coef (se) = -0.0498912 (0.0158296)
```

```r
set.seed(1)
DML1.RF = DML1.for.PLM(x, d, y, dreg, yreg, nfold=5)
```

```
## fold: 1  2  3  4  5  
## coef (se) = -0.0504166 (0.0152801)
```
# Compare Forest vs Lasso


```r
comp.tab= matrix(NA, 2, 2)
comp.tab[1,] = c( sqrt(var(DML2.RF$ytil)),  sqrt(var(DML2.lasso$ytil)))
comp.tab[2,] = c( sqrt(var(DML2.RF$dtil)),  sqrt(var(DML2.lasso$dtil)))
rownames(comp.tab) = c("RMSE for Y:", "RMSE for D:")
colnames(comp.tab) = c("RF", "LASSO")
print(comp.tab, digits=3)
```

```
##                 RF  LASSO
## RMSE for Y: 0.0483 0.0532
## RMSE for D: 0.3701 0.3822
```

Conclude that Forest is (slighly) better


# DML for Parially Linear IV Models

We shall subsume the analysis of inference in the partially linear model as a special case of the partially linear structural equation model:
\begin{eqnarray}
 &  Y - D\theta_0 = g_0(X) + \zeta,  & E[\zeta \mid Z,X]= 0,\\
  & Z = m_0(X) +  V,   &  E[V \mid X] = 0. 
\end{eqnarray}
Note that this model is not a regression model unless $Z=D$.  Model  is a canonical
model in causal inference, going back to S. Wright's work on IV methods for estimaing demand/supply equations, with the modern difference being that $g_0$ and $m_0$ are nonlinear, potentially complicated functions of high-dimensional $X$.  The idea of this model is that there is a structural or causal relation between $Y$ and $D$, captured by $\theta_0$, and $g_0(X) + \zeta$ is the stochastic error, partly explained by covariates $X$. 
$V$ and $\zeta$ are stochastic errors that are not explained by $X$. Since $Y$ and $D$ are jointly determined, we need an external factor, commonly referred to as an instrument, $Z$ to create exogenous variation
in $D$.   Note that $Z$ should affect $D$.  The $X$ here serve again as confounding factors, so we can think of variation in $Z$ as being exogenous only conditional on $X$. 

---

# Example

A simple contextual example is from biostatistics, where $Y$ is a health outcome and $D$ is indicator of smoking.  Thus, $\theta_0$ is captures the effect of smoking on health.  Health outcome $Y$ and smoking behavior $D$ are treated as being jointly determined.  $X$ represents patient characteristics, and $Z$ could be a doctor's advice not to smoke (or another behavioral treatment) that may affect the outcome $Y$ only through shifting the behavior $D$, conditional on characteristics $X$.   

----

# PLIVM in Residualized Form

The PLIV model above can be rewritten in the following residualized form:
$$
  W = U \theta_0 + \zeta,   \quad  E[\zeta \mid V,X]= 0,\\
 W = (Y- \ell_0(X)),  \quad \ell_0(X) = E[Y \mid X] \\
   U= (D - r_0(X)), \quad r_0(X) = E[D \mid X] \\
   V = (Z- m_0(X)), \quad m_0(X) = E[Z \mid X].
$$
   The variables above represent original variables after taking out or ``partialling out"
  the effect of $X$.  Note that $\theta_0$ is identified from this equation if $V$ 
  and $U$ have non-zero correlation, which automatically means that $U$ and $V$
  must have non-zero variation.

-----

Given identification, DML  proceeds
as follows

  1.  Estimate $\ell_0$, $r_0$, and $m_0$ by $\hat \ell_0$, $\hat r_0$, and $\hat m_0$, which amounts
to solving the three problems of predicting $Y$, $D$, and $Z$ using $X$, using any generic 
ML method, giving us estimated residuals 

$$\hat W= Y - \hat \ell_0(X), \\ \hat U= D - \hat r_0(X), \\ \hat V = Z- \hat m_0(X).$$ The estimates
should be of a cross-validated form, as explained in the algorithm below. 

2. Estimate $\theta_0$ by the the intstrumental
variable regression of $\hat W$ on $\hat U$ using $\hat V$ as an instrument.
Use the conventional inference for the IV regression estimator, ignoring
the estimation error in these residuals. 

As before, the reason we work with this residualized form is that it eliminates the bias
arising when solving the prediction problem in stage 1.  The estimator is adaptive,
in the sense that the first stage estimation errors do not affect the second 
stage errors.



----



# DML for PLIVM


```r
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
```
----
   
   # AJR Data


```r
data(AJR); 
y = AJR$GDP; 
d = AJR$Exprop; 
z = AJR$logMort
x = model.matrix(~ -1 + (Latitude + Latitude2 + Africa + 
                           Asia + Namer + Samer)^2, data=AJR)
dim(x)
```

```
## [1] 64 21
```

* Y is log GDP;
* D is a measure of Protection from Expropriation, a proxy for 
quality of insitutions;
* Z is the log of Settler's mortality;
* W are geographical variables (latitude, latitude squared, continent dummies as well as interactions)


# DML with Random Forest


```r
dreg <- function(x,d){ randomForest(x, d) }  #ML method=Forest
yreg <- function(x,y){ randomForest(x, y) }  #ML method=Forest
zreg<-  function(x,z){ randomForest(x, z)}   #ML method=Forest 
  
set.seed(1)
DML2.RF = DML2.for.PLIVM(x, d, z, y, dreg, yreg, zreg, nfold=20)
```

```
## fold: 1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  
## coef (se) = 0.919031 (0.434487)
```

# DML with PostLasso


```r
dreg <- function(x,d){ rlasso(x, d, post=T) }  #ML method=lasso
yreg <- function(x,y){ rlasso(x, y, post=T) }  #ML method=lasso
zreg<-  function(x,z){ rlasso(x, z, post=T)}  #ML method=lasso 

set.seed(1)
DML2.lasso = DML2.for.PLIVM(x, d, z, y, dreg, yreg, zreg, nfold=20)
```

```
## fold: 1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  
## coef (se) = 0.776972 (0.201375)
```

# Compare Forest vs Lasso

```r
comp.tab= matrix(NA, 3, 2)
comp.tab[1,] = c( sqrt(var(DML2.RF$ytil)),  sqrt(var(DML2.lasso$ytil)))
comp.tab[2,] = c( sqrt(var(DML2.RF$dtil)),  sqrt(var(DML2.lasso$dtil)))
comp.tab[3,] = c( sqrt(var(DML2.RF$ztil)),  sqrt(var(DML2.lasso$ztil)))
rownames(comp.tab) = c("RMSE for Y:", "RMSE for D:", "RMSE for Z:")
colnames(comp.tab) = c("RF", "LASSO")
print(comp.tab, digits=3)
```

```
##                RF LASSO
## RMSE for Y: 0.808 0.919
## RMSE for D: 1.352 1.603
## RMSE for Z: 0.961 1.087
```

* RF seems to perform better and it is preferrable to use it

* The reason is that the size of second order terms for better ML methods.








