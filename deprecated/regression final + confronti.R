setwd("C:/Users/panze/Desktop/NONPARAM STATISTICS/PROGETTO_GITHUB/NPS")

rm(list=ls())
library(MASS)
library(car)
library(Metrics)
library(dplyr)
library(np)
library(gam)
library(robustbase)
library(lme4)
library(pbapply)

df = read.csv("kc_cleaned.csv")
attach(df)
target = "log10.price."

#############################################################################
# Define functions
#############################################################################

train_test_split <- function(df, perc_train = 0.8){
  set.seed(0401)
  train_ind <- sample(seq_len(nrow(df)), size = perc_train*nrow(df))
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  return(list(x_train=train[, !names(df) %in% target], y_train=train[,target], 
              x_test=test[, !names(df) %in% target], y_test=test[,target]))
}

select_columns <- function(...){
  return(list(x_train=x_train[,c(...)],x_test=x_test[,c(...)]))
}

eval_regr <- function(model, x_test, y_test, type="mape"){
  #predictors = labels(terms(model))
  y_pred = predict(model,x_test)
  if (type=="mae"){
    error = mae(10^y_test, 10^y_pred) 
    print(paste("Mean Absolute Error is", round(error), "dollars"))
  }
  else if (type=="mape"){
    error = mape(10^y_test, 10^y_pred) 
    print(paste("Mean Absolute Percentage Error is", round(100*error,2), "%"))
  }
  else if (type=="mse"){
    error = mse(10^y_test, 10^y_pred) 
    print(paste("Mean Squared Error is", round(error), "dollars^2"))
  }
  else if (type=="rmse"){
    error = rmse(10^y_test, 10^y_pred) 
    print(paste("Root Mean Squared Error is", round(error), "dollars"))
  }
  else{
    print("Not implemented")
  }
  #return(error)
}

#############################################################################
# Split train and test
#############################################################################

split = train_test_split(df)
x_train = split$x_train; y_train = split$y_train
x_test = split$x_test; y_test = split$y_test;

#############################################################################
# Variable groups
#############################################################################

useless = c("id","price","bedrooms","bathrooms","floors",
            "waterfront","has_bas")
useless_age = c("date", "yr_built", "yr_renovated","renovate_index")
useless_geo = c("zipcode", "lat", "long")
useless_sqm = c("sqm_living", "sqm_lot", "sqm_living15", "sqm_lot15", "sqm_above",
                "sqm_basement")

useful_gen = c("bedfloors_ratio", "bathfloors_ratio","ord_date","view","condition",
               "grade","is_rich")
useful_age = c("has_ren", "yr_old")  # either them or renovate_index
useful_geo = c("geodist_index")
useful_sqm = c("log10.sqm_living.","log10.sqm_lot.","log10.sqm_living15.",
               "log10.sqm_lot15.")

#############################################################################
# Multivariate Regressions
#############################################################################

selector = select_columns(useful_gen[c(-1,-3)],useful_age,useful_geo,useful_sqm)
X1 = selector$x_train; x_test_1 = selector$x_test
model_final = lmrob(y_train~ns(bathfloors_ratio, df=2)+
                      view + grade +
                      cut(condition,breaks = c(min(condition),3,max(condition)),include.lowest = T, right=F)+
                      I((yr_old-80)*(yr_old>80)) + yr_old:has_ren + 
                      bs(geodist_index, degree=2) +
                      log10.sqm_living. +
                      log10.sqm_lot.+
                      log10.sqm_living15.+
                      log10.sqm_lot15.+is_rich
                    ,data=X1
)  # Using lmrob's MM-type estimator for lm
summary(model_final)
eval_regr(model_final,x_test_1,y_test, "mae") # Ignore warning (it's because the test data has geodists that are lower than the minimum found in the train data)
eval_regr(model_final,x_test_1,y_test, "mape")
plot(model_final)

###################################################################################################################################################################################

linmod <- lm(y_train ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
               log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1)
summary(linmod)
x11()
plot(linmod$residuals,xlab='Fitted',ylab='Residuals', main='Residuals Vs Fitted - Classic Model')
x11()
qqnorm(linmod$residuals)
qqline(linmod$residuals)

#Global F-test
n <- dim(X1)[1]
T0_glob <- summary(linmod)$f[1]
T0_glob
B <- 1000 
T_H0glob <- numeric(B)
for(perm in 1:B){
  permutazione <- sample(n)
  Y.perm.glob <- y_train[permutazione]
  T_H0glob[perm] <- summary(lm(Y.perm.glob ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
                                 log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1))$f[1]
}
p_val <- sum(T_H0glob>=T0_glob)/B
p_val #0

#t-test su log10.sqm_lot.
#H0: beta8 = 0 vs H1: beta8 != 0
T0_x1 <- abs(summary(linmod)$coefficients[9,3])
T0_x1
regr.H01 <- lm(y_train ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
                    log10.sqm_living. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1)
residui.H01 <- regr.H01$residuals
T_H01 <- numeric(B)
for(perm in 1:B){
  permutazione <- sample(n)
  residui.H01.perm <- residui.H01[permutazione]
  Y.perm.H01 <- regr.H01$fitted + residui.H01.perm
  T_H01[perm] <- abs(summary(lm(Y.perm.H01 ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
                                  log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1))$coefficients[9,3])
}
p_val <- sum(T_H01>=T0_x1)/B
p_val #0

#t-test su log10.sqm_lot15.
#H0: beta10 = 0 vs H1: beta10 != 0
T0_x2 <- abs(summary(linmod)$coefficients[11,3])
T0_x2
regr.H02 <- lm(y_train ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
                 log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + is_rich, data=X1)
residui.H02 <- regr.H02$residuals
T_H02 <- numeric(B)
for(perm in 1:B){
  permutazione <- sample(n)
  residui.H02.perm <- residui.H02[permutazione]
  Y.perm.H02 <- regr.H02$fitted + residui.H02.perm
  T_H02[perm] <- abs(summary(lm(Y.perm.H02 ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
                                  log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1))$coefficients[11,3])
}
p_val <- sum(T_H02>=T0_x2)/B
p_val #0.023


## Two models comparison

print("AIC classical")
AIC(linmod)
print("AIC Nonparam")
AIC(model_final)

print("BIC classical")
BIC(linmod)
print("BIC Nonparam")
BIC(model_final)

library(qpcR)
print("AICc classical")
AICc(linmod)
print("AICc Nonparam")
AICc(model_final)

eval_regr(model_final,x_test_1,y_test, "mae") 
eval_regr(linmod,x_test_1,y_test, "mae") 
eval_regr(model_final,x_test_1,y_test, "mape")
eval_regr(linmod,x_test_1,y_test, "mape")


##############################################################################################################################################################


##Prediction with the two models

pred_class <- predict(linmod, newdata=x_test_1, type='response')
pred_nonparam <- predict(model_final, newdata=x_test_1, type='response')

#Reverse percentile intervals

B<-1000
pred.boot.L <- numeric(B)
fitted.obs<-model_final$fitted.values
res.obs <-model_final$residuals

for(b in 1:B)
{
  response.b=fitted.obs + sample(res.obs, replace=T)
  response.b=as.vector(response.b)
  modfin.b = lmrob(response.b~ns(bathfloors_ratio, df=2)+
                              view + grade +
                              cut(condition,breaks = c(min(condition),3,max(condition)),include.lowest = T, right=F)+
                              I((yr_old-80)*(yr_old>80)) + yr_old:has_ren + 
                              bs(geodist_index, degree=2) +
                              log10.sqm_living. +
                              log10.sqm_lot.+
                              log10.sqm_living15.+
                              log10.sqm_lot15.+is_rich,
                   data=X1)
  pred.boot.L[b] <- predict(modfin.b, newdata=x_test_1, type='response')
}


#wrapper=function(){
#  response.b=fitted.obs + sample(res.obs, replace=T)
#  response.b=as.vector(response.b)
#  modfin.b = lmrob(response.b~ns(bathfloors_ratio, df=2)+
#                   view + grade +
#                   cut(condition,breaks = c(min(condition),3,max(condition)),include.lowest = T, right=F)+
#                   I((yr_old-80)*(yr_old>80)) + yr_old:has_ren + 
#                   bs(geodist_index, degree=2) +
#                   log10.sqm_living. +
#                   log10.sqm_lot.+
#                   log10.sqm_living15.+
#                   log10.sqm_lot15.+is_rich,
#                 data=X1)
#  pred.b <- predict(modfin.b, newdata=x_test_1, type='response')
#}
#pred.boot.L<-pbreplicate(B,wrapper(),simplify='vector')

alpha <- 0.05
T.boot <- pred.boot.L
T.obs <- as.vector(pred_nonparam)
r.q.L<- quantile(T.boot, 1-alpha/2)
l.q.L<-quantile(T.boot, alpha/2)
CI<-cbind(T.obs-(r.q.L-T.obs), T.obs, T.obs-(l.q.L-T.obs))


