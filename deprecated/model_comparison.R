#########################################################################
# Model comparison
#########################################################################
# compare to lm..easy
# TODO: compute the exact same variables as lm?
# xgboost is non-param
train_XGB <- function(X_train, y_train, X_test, y_test){
  x_train_ = data.matrix(X_train)
  y_train_ = data.matrix(y_train)
  x_test_ = data.matrix(X_test)
  y_test_ = data.matrix(y_test)
  dtrain = xgb.DMatrix(data=x_train_, label = y_train_)
  model_param = xgboost(data=dtrain, label = y_train, nrounds=750,
                        eval_metric = "mape", booster="gbtree")
  
  summary(model_param)
  eval_regr(model_param,x_test_,y_test_, "mae") # Ignore warning (it's because the test data has geodists that are lower than the minimum found in the train data)
  eval_regr(model_param,x_test_,y_test_, "mape")
  eval_regr(model_param,x_test_,y_test_, "rmse")
  # plot them features! what's contributing most to our model?
  xgb.plot.multi.trees(feature_names = names(x_train_), 
                       model = model_param)
  # get information on how important each feature is
  importance_matrix <- xgb.importance(names(x_train_), model = model_param)
  # and plot it!
  xgb.plot.importance(importance_matrix)
  return(model_param)
}
model_param = train_XGB(x_train[4:34],y_train,x_test[4:34],y_test)



x_train_ = data.matrix(x_train)
y_train_ = data.matrix(y_train)
x_test_ = data.matrix(x_test)
y_test_ = data.matrix(y_test)
dtrain = xgb.DMatrix(data=x_train_, label = y_train_)
dvalid = xgb.DMatrix(data=x_test_, label = y_test_)


# Take start time to measure time of random search algorithm
start.time <- Sys.time()

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(20)
for (iter in 1:100){
  param <- list(
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)

# Use randomly created parameters to create 10,000 XGBoost-models
for (row in 1:nrow(parameters_df)){
  set.seed(20)
  mdcv <- xgb.train(data=dtrain,
                    booster = "gbtree",
                    objective = "reg:squarederror",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 500,
                    eval_metric = "mae",
                    early_stopping_rounds= 30,
                    print_every_n = 500,
                    watchlist = list(train= dtrain, val= dvalid)
  )
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_error))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)

# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_error)`)

# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

write.csv(randomsearch, "randomsearch.csv")
