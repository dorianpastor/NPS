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
library(xgboost)
library(Hmisc)

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
  y_pred = predict(model,x_test)
  if (type=="mae"){
    error = round(mae(10^y_test, 10^y_pred)) 
    print(paste("Mean Absolute Error is", error, "dollars"))
  }
  else if (type=="mape"){
    error = round(100*mape(10^y_test, 10^y_pred),2)
    print(paste("Mean Absolute Percentage Error is", error, "%"))
  }
  else if (type=="mse"){
    error = round(mse(10^y_test, 10^y_pred))
    print(paste("Mean Squared Error is", error, "dollars^2"))
  }
  else if (type=="rmse"){
    error = round(rmse(10^y_test, 10^y_pred))
    print(paste("Root Mean Squared Error is", error, "dollars"))
  }
  else{
    print("Not implemented")
  }
  return(error)
}


train_XGB <- function(X_train, y_train, X_test, y_test){
  x_train_ = data.matrix(X_train)
  y_train_ = data.matrix(y_train)
  x_test_ = data.matrix(X_test)
  y_test_ = data.matrix(y_test)
  dtrain = xgb.DMatrix(data=x_train_, label = y_train_)
  model_param = xgboost(data=dtrain, label = y_train, nrounds=250,
                        objective = "reg:squarederror",
                        booster="gbtree")
  
  summary(model_param)
  eval_regr(model_param,x_test_,y_test_, "mae") 
  eval_regr(model_param,x_test_,y_test_, "mape")
  # plot them features! what's contributing most to our model?
  xgb.plot.multi.trees(feature_names = names(x_train_), 
                       model = model_param)
  # get information on how important each feature is
  importance_matrix <- xgb.importance(names(x_train_), model = model_param)
  # and plot it!
  xgb.plot.importance(importance_matrix)
  return(model_param)
}

#############################################################################
# Split train and test
#############################################################################

split = train_test_split(df)
x_train = split$x_train; y_train = split$y_train
x_test = split$x_test; y_test = split$y_test;


#############################################################################
# Feature groups
#############################################################################

useless = c("id","price","bedrooms","bathrooms","floors",
            "waterfront","has_bas")
useless_age = c("date", "yr_built", "yr_renovated","renovate_index")
useless_geo = c("zipcode", "lat", "long")
useless_sqm = c("sqm_living", "sqm_lot", "sqm_living15", "sqm_lot15", "sqm_above",
                "sqm_basement")

useful_gen = c("bedfloors_ratio", "bathfloors_ratio","ord_date","view","condition",
               "grade","is_rich")
useful_age = c("has_ren", "yr_old")
useful_geo = c("geodist_index")
useful_sqm = c("log10.sqm_living.","log10.sqm_lot.","log10.sqm_living15.",
               "log10.sqm_lot15.")


#############################################################################
# Final NonParametric Regression Model
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
                    ,data=X1)  # Using lmrob's MM-type estimator for lm
summary(model_final)
plot(model_final)


###################################################################################################################################################################################
# Comparison 1 - Parametric Model
# Linear Regression (with the same variables)
###################################################################################################################################################################################

linmod <- lm(y_train ~ bathfloors_ratio + view + grade + condition + yr_old + yr_old:has_ren + geodist_index +
               log10.sqm_living. + log10.sqm_lot. + log10.sqm_living15. + log10.sqm_lot15. + is_rich, data=X1)
summary(linmod)
plot(linmod)

###################################################################################################################################################################################
# Testing significance of linear regression model
###################################################################################################################################################################################

#Global F-test 
n <- dim(X1)[1]
T0_glob <- summary(linmod)$f[1]
T0_glob
B <- 10000 
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

# We did a permutational t-test on the two variables that were more likely to 
# be not significative because of their higher pvalues on the summary

###################################################################################################################################################################################
# Comparison 2 - Other Non-Parametric Model
# XGBoost (with the same variables)
###################################################################################################################################################################################
# We train it on all variables
model_param = train_XGB(x_train[4:34],y_train,x_test[4:34],y_test)


###################################################################################################################################################################################
# Comparison Results
###################################################################################################################################################################################

#Mean Absolute Error
eval_regr(model_final,x_test_1,y_test, "mae") 
eval_regr(linmod,x_test_1,y_test, "mae") 
eval_regr(treemod,x_test_1,y_test, "mae") 

#Mean Absolute Percentage Error
eval_regr(model_final,x_test_1,y_test, "mape")
eval_regr(linmod,x_test_1,y_test, "mape")
eval_regr(treemod,x_test_1,y_test, "mape") 

#Mean Squared Error
eval_regr(model_final,x_test_1,y_test, "mse")
eval_regr(linmod,x_test_1,y_test, "mse")
eval_regr(treemod,x_test_1,y_test, "mse") 

#Root Mean Squared Error
eval_regr(model_final,x_test_1,y_test, "rmse")
eval_regr(linmod,x_test_1,y_test, "rmse")
eval_regr(treemod,x_test_1,y_test, "rmse") 

#Prediction with classical method
pred_lin <- predict(linmod, newdata=x_test_1, type='response')
pred_nonparam <- predict(model_final, newdata=x_test_1, type='response')

###################################################################################################################################################################################
# Diagnostics on the NonParametric Regression
###################################################################################################################################################################################
# How does error change as a function of the house prices?

# log($) error as function of the price
y_pred = predict(model_final,X_test)
plot(y_test, y_test-y_pred)
abline(h=0)


n_windows = 100 # Data are split in buckets of n_windows elements

# In this block buckets are based on absolute error
data = data.frame(cbind(y_test, abs(10^y_test-10^y_pred)))
colnames(data) = c("y_test","abs_error")
data$group =  as.numeric(cut_number(data$y_test,n_windows))
datagg = data[,2:3] %>% 
  group_by(group) %>% 
  summarise(across(everything(), list(mean = mean)))
plot(datagg, pch=20, xlab="bins from 100k to 2M dollar houses", ylab="mae in each bin")
min(datagg[,2])  # Best performing window of prediction average error
max(datagg[,2])  # Worst performing window of prediction average error
# Distribution of pct error (having the mean error is not satisfying)
hist(as.numeric(unlist(unname(datagg[,2]))), breaks=20) 

# In this block buckets are based on absolute percentage error
data_pct = data.frame(cbind(y_test, 100*abs(10^y_test-10^y_pred)/(10^y_pred)))
colnames(data_pct) = c("y_test","abs_pct_error")
data_pct$group =  as.numeric(cut_number(data_pct$y_test,n_windows))
datagg_pct = data_pct[,2:3] %>% 
  group_by(group) %>% 
  summarise(across(everything(), list(mean = mean)))
plot(datagg_pct, pch=20, xlab="bins from 100k to 2M dollar houses", ylab="mape in each bin")
grid(nx=n_windows, ny=NULL)
min(datagg_pct[,2])  # Best performing window of prediction average pct error
max(datagg_pct[,2])  # Worst performing window of prediction average pct error
# Distribution of pct error (having the mean error is not satisfying)
hist(as.numeric(unlist(unname(datagg_pct[,2]))), breaks=20) 

# Where should our model for expensive houses be applied?
datagg_pct[datagg_pct[,2]>20,1] 
# first 5 groups, last 4 (or 9) out of 100
groupsize = dim(x_test)[1]/n_windows
sorted_by_price = x_test[order(x_test$price),"price"]
max(sorted_by_price[1:5*groupsize])  # until 210k
# All the houses less expensive than 210k $ should be modelled differently
sorted_by_price = x_test[order(x_test$price, decreasing = T),"price"]
min(sorted_by_price[1:6*groupsize]) # 1.2M (or 900k) (to have 1M, last 6)
# We decide to model all the houses more expensive than 1M $ with a different model

###################################################################################################################################################################################
# Model for expensive properties
###################################################################################################################################################################################

X2 = X1[which(y_train>=log10(1000000)),] ; x_test_2 = x_test_1[which(y_test>=log10(1000000)),]
y_train_2 = y_train[which(y_train>=log10(1000000))]; y_test_2 = y_test[which(y_test>=log10(1000000))]
model_expensive = lmrob(y_train_2~ns(bathfloors_ratio, df=2)+
                      view + grade +
                      bs(geodist_index, degree=2) +   
                      bs(log10.sqm_living., degree=2) + 
                      bs(log10.sqm_lot., degree=2) +   
                      log10.sqm_living15.+             
                      ns(log10.sqm_lot15., df=2)
                    , data=X2
) 
mae_exp = eval_regr(model_expensive,x_test_2,y_test_2, "mae")
mape_exp = eval_regr(model_expensive,x_test_2,y_test_2, "mape")



###################################################################################################################################################################################
# Model for standard properties
###################################################################################################################################################################################

X0 = X1[which(y_train<log10(1000000)),] ; x_test_0 = x_test_1[which(y_test<log10(1000000)),]
y_train_0 = y_train[which(y_train<log10(1000000))]; y_test_0 = y_test[which(y_test<log10(1000000))]
model_standard = lmrob(y_train_0~ns(bathfloors_ratio, df=2)+
                      view + grade +
                      cut(condition,breaks = c(min(condition),3,max(condition)),include.lowest = T, right=F)+
                      I((yr_old-80)*(yr_old>80)) + yr_old:has_ren + 
                      bs(geodist_index, degree=2) +
                      log10.sqm_living. +
                      log10.sqm_lot.+
                      log10.sqm_living15.+
                      log10.sqm_lot15.+is_rich
                    ,data=X0) 
mae_st = eval_regr(model_standard,x_test_0,y_test_0, "mae")
mape_st = eval_regr(model_standard,x_test_0,y_test_0, "mape")

###################################################################################################################################################################################
# Merged results
###################################################################################################################################################################################

final_mape = (length(y_test_0)*mape_st+length(y_test_2)*mape_exp)/length(y_test)
final_mape  # 17%
final_mae = (length(y_test_0)*mae_st+length(y_test_2)*mae_exp)/length(y_test)
final_mae  # 75k on standard 200k on expensive

###################################################################################################################################################################################
# Prediction with Reverse Percentile Intervals 
###################################################################################################################################################################################

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

alpha <- 0.05
T.boot <- pred.boot.L
T.obs <- as.vector(pred_nonparam)
r.q.L<- quantile(T.boot, 1-alpha/2)
l.q.L<-quantile(T.boot, alpha/2)
CI<-cbind(T.obs-(r.q.L-T.obs), T.obs, T.obs-(l.q.L-T.obs))

#NON CICLA PD MA E' GIUSTO, BISOGNA CAPIRE PERCHE' NON VA CON lmrob()

x11()
layout(1)
plot(CI[,2], col='darkgreen', cex=1, xlim=c(1,30),ylim=c(5,6),pch=17, main='Classic Vs Nonparametric', xlab='', ylab='LogY')
points(CI[,1], col='blue', cex=1.7, pch='-')
points(CI[,3], col='blue', cex=1.7, pch='-')
segments(1:30,CI[,1],1:30,CI[,3], lwd=1)
points(pred_nonparam, col='red', cex=1,pch=15)
points(y_test, col='black', cex=1, pch=19)
legend('topleft',c("nonparam", "classic", "true"), col=c('darkgreen','red','black'), pch=c(17,15,19))


###################################################################################################################################################################################
# 
###################################################################################################################################################################################



