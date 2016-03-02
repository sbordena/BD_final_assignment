## Final Project Script
## Load libraries
library(tree)
library(rpart)
library(randomForest)
library(gbm)
library(data.table)
library(caret)
library(doParallel)
library(glmnet)
library(e1071)
library(pROC)
library(curl)
library(dismo)
library(distrom)
library(gamlr)
library(dplyr)
library(Matrix)

## Set Working Directory
setwd("C:/Users/Chingono/Documents/Big Data Course")

## Get Functions
source("https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master/lift.R")
helpr_repo_raw_url <- 'https://raw.githubusercontent.com/ChicagoBoothML/HelpR/master'
source(file.path(helpr_repo_raw_url, 'EvaluationMetrics.R'))
source("naref.R")
source("fdr.R")
source("roc.R")
source("deviance.R")

##Parallel Computing
# set randomizer's seed
set.seed(28)
# Parallel Computation Setup
cl <- makeCluster(detectCores()-2)   # create a compute cluster using all CPU cores except 2
clusterEvalQ(cl, library(foreach))
registerDoParallel(cl)   # register this cluster

###load data
data=read.csv("equities_data.csv") 
data <- data[complete.cases(data),]

### To read the quarterly data, install the "foreign" and "readstata13" packages 
### and un-comment the following:
# library(foreign)
# library(readstata13)
# quarterly_data <- read.dta("Quarterly_Fundamentals_2.17.16.dta")
# monthly_returns <- read.dta("Monthly Returns 2.17.16.dta")

## Create factors among explanatory variables
data$size_index <- as.factor(data$size_index)
data$pyreturn_index <- as.factor(data$pyreturn_index)
data$value_index <- as.factor(data$value_index)
data$profit_index <- as.factor(data$profit_index)
data$pb_index <- as.factor(data$pb_index)
data$pyreturn_below_median <- as.factor(data$pyreturn_below_median)
data$asset_turnover <- as.factor(data$asset_turnover)
data$debtpy_debt <- as.factor(data$debtpy_debt)
data$sharespy_shares <- as.factor(data$sharespy_shares)

#split data into train, validate, and test
set.seed(28)
n=nrow(data)
n1=floor(0.6*n)
n2=floor(0.2*n)
n3=n-n1-n2
ii=sample(1:n,n)
trainDf=data[ii[1:n1],]
valDf=data[ii[n1+1:n2],]
testDf=data[ii[n1+n2+1:n3],]

###check that samples are balanced in terms of y (Future Debt Paydown)
table(trainDf$y)
table(valDf$y)
table(testDf$y)
## ~42% of companies in each sample pay down debt over the next year

### Logistic Regression ###

###fit simple logit model
lgfit = glm(y~.,data=trainDf[,-27:-28],family=binomial) # exclude next 1 year return and portfolio year (column 27 & 28)
summary(lgfit)
pv <- coef(summary(lgfit))[,4] #grab p-values
length(pv[pv<=0.05]) # 36 coefficients appear to be significant at the 5% level

## Prediction with simple logit model
p_logit <- predict(lgfit, newdata=valDf, type="response")
D <- deviance(y=valDf$y, pred=p_logit, family="binomial")
ybar <- mean(valDf$y==1) # marginal prob(y==1)
D0 <- deviance(y=valDf$y, pred=ybar, family="binomial")

## OOS R-squared is 5.23%
1 - D/D0

## Plot a couple of Variables that appear to be related to y (Future Debt Paydown)
## We see that:
## 1a. Most companies have low leverage (Long Term Debt/Enterprsie Value)
## 1b. There is a monotonic decrease in frequency of observations as LT Debt/EV increases
## 2. On average, companies that did NOT pay down debt last year have higher LT Debt/Assets
## 3. On average, companies that DID pay down debt last year have higher Gross Profitability

hist(trainDf$lt_debt_ev,
     main="Long Term Debt / Enterprise Value",
     xlab="LT Debt / EV")

boxplot(debt_assets~debtpy_debt, data=trainDf, ylim=c(0,1),
        main="LT Debt / Assets by Prior Debt Paydown",
        xlab="Prior Debt Paydown", ylab="LT Debt / Assets")

boxplot(gprofit_assets~debtpy_debt, data=trainDf, ylim=c(-0.5,1),
        main="Gross Profitability by Prior Debt Paydown",
        xlab="Prior Debt Paydown", ylab="Gross Profit / Assets")

## FDR COntrol
## Plot the p-values
hist(pv,col=8, main="Histogram of P-Values") 
# We see a big spike at zero, indicating more p-values near zero than one
# would expect if they all came from the null. There is signal in the data.

# FDR Cut at 0.01:
alpha <- fdr_cut(pv,q=.01,plotit=TRUE)
alpha ## alpha cutoff is 0.001070771
signif <- which(pv <= alpha) ## which are significant
length(signif)  ## 26 coefficients (out of the original 36) are significant after FDR control

###fit logit model with interactions
x_train <- trainDf[,-26:-28]
x_train_sp <- sparse.model.matrix(~ .^2, data=x_train)[,-1] #This takes a while!

x_val <- valDf[,-26:-28]
x_val_sp <- sparse.model.matrix(~ .^2, data=x_val)[,-1]

x_test <- testDf[,-26:-28]
x_test_sp <- sparse.model.matrix(~ .^2, data=x_test)[,-1]

## Lasso & CV Regression (both models take a few minutes to run)
y_train <- trainDf$y
lasso_reg <- gamlr(x_train_sp,y_train, family="binomial", lmr=1e-4)
plot(lasso_reg)

lasso.cv <- cv.gamlr(x_train_sp,y_train, family="binomial", lmr=1e-4, verb=TRUE)
plot(lasso.cv)

# CV OOS R-Squared is higher than the simple GLM model
(1- lasso.cv$cvm[lasso.cv$seg.1se]/lasso.cv$cvm[1]) # 1se rule = 6.70%
(1- lasso.cv$cvm[lasso.cv$seg.min]/lasso.cv$cvm[1]) # min rule = 6.81% 

### Set up Deviance Loss Function to get OOS R2 of Lasso Regression###
###deviance loss function
lossf = function(y,phat,wht=0.0000001) {
  #y should be 0/1
  #wht shrinks probs in phat towards .5, don't log 0!
  if(is.factor(y)) y = as.numeric(y)-1
  phat = (1-wht)*phat + wht*.5
  py = ifelse(y==1,phat,1-phat)
  return(-2*sum(log(py)))
}

# Lasso reg OOS R-squared
p_lasso_reg <- predict(lasso_reg, newdata=x_val_sp, type="response")
phat_lasso = matrix(p_lasso_reg,ncol=1)
D <- lossf(y=valDf$y, phat=phat_lasso)
ybar <- mean(valDf$y==1) # marginal prob(y==1)
D0 <- deviance(y=valDf$y, pred=ybar, family="binomial")

## Lasso Reg's OOS R-squared is 6.69% (also higher than simple logit)
1 - D/D0


### Principal Component Analysis ###
## Turn Factors to numeric to do PCA
x_train$size_index <- as.numeric(x_train$size_index)
x_train$pyreturn_index <- as.numeric(x_train$pyreturn_index)
x_train$value_index <- as.numeric(x_train$value_index)
x_train$profit_index <- as.numeric(x_train$profit_index)
x_train$pb_index <- as.numeric(x_train$pb_index)
x_train$pyreturn_below_median <- as.numeric(x_train$pyreturn_below_median)
x_train$asset_turnover <- as.numeric(x_train$asset_turnover)
x_train$debtpy_debt <- as.numeric(x_train$debtpy_debt)
x_train$sharespy_shares <- as.numeric(x_train$sharespy_shares)

pca_train <- prcomp(x_train, scale=TRUE)
z_train <- predict(pca_train)

## plot and interpret
plot(pca_train, main="")
mtext(side=1, "Equity PCs",  line=1, font=2)
round(pca_train$rotation[,1:4],1) 

## PC1 captures the size factor (see loadings on big stocks)
## PC2 captures low leverage, large growth stocks with positive momentum
## PC3 capturs low leverage, small growth stocks with negative momentum
## PC4 captures value stocks with high profitability

save.image()

```{r}
stopCluster(cl)   # shut down the parallel computing cluster
```