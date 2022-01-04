rm(list=ls())
library(MASS)
library(Metrics)
library(dplyr)
library(np)
library(gam)
library(robustbase)

df = read.csv("kc_cleaned.csv")
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
  predictors = labels(terms(model))
  y_pred = predict(model,x_test)
  if (type=="mape"){
    error = mape(y_test, y_pred) 
    print(paste("Mean Absolute Percentage Error is", 100*error, "%"))
  }
  else if (type=="mse"){
    error = mse(y_test, y_pred) 
    print(paste("Mean Squared Error is", error))
  }
  else if (type=="rmse"){
    error = rmse(y_test, y_pred) 
    print(paste("Root Mean Squared Error is", error))
  }
  else{
    print("Not implemented")
  }
  return(error)
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
               "grade")
useful_age = c("has_ren", "yr_old")  # either them or renovate_index
useful_geo = c("geodist_index")
useful_sqm = c("log10.sqm_living.","log10.sqm_lot.","log10.sqm_living15.",
               "log10.sqm_lot15.")

#############################################################################
# Multivariate Regressions
#############################################################################

selector = select_columns(useful_gen,"renovate_index",useful_geo,useful_sqm)
X1 = selector$x_train; x_test_1 = selector$x_test

fit_lms = lmsreg(y_train~., X1)	 # Least Median Squares
eval_regr(fit_lms,x_test_1,y_test)

fit_lm = lm(y_train~., X1)	      # OLS
eval_regr(fit_lm,x_test_1,y_test)
