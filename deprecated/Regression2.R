library(dplyr)
library(MASS)
library(Metrics)

df = read.csv("kc_cleaned.csv")
attach(df)

# "id"  "date"  "price" "zipcode" "lat" "long"  "yr_built"  "yr_renovated"
# "sqm_living"   "sqm_lot"   "sqm_above"   "sqm_basement"   "sqm_living15"   "sqm_lot15" 
# "bedrooms"  "bathrooms" "bedfloors_ratio" bathfloors_ratio" "floors"  "waterfront"  "view" "geodist_index"           
# "condition" "grade" "yr_old"  "renovate_index"  "has_ren" "has_bas" "ord_date"
# "log10.sqm_living."  "log10.sqm_lot."  "log10.sqm_living15."  "log10.sqm_lot15."
# "log10.price." 

# Useless: "id", "date", "yr_built", "yr_renov", "zipcode", "lat", "long", "price"
# We use the logs of "sqm_living", "sqm_lot", "sqm_living15", "sqm_lot15" instead of the originals

# Target: "log10.price."

#############################################################################
# Define functions
#############################################################################

train_test_split <- function(df, perc_train = 0.8){
train_ind <- sample(seq_len(nrow(df)), size = perc_train*nrow(df))
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  return(list(train=train, test=test))
}

eval_regr <- function(model, test, type="mape"){
  predictors = labels(terms(model))
  y_test = test$log10.price.
  x_test = test[,predictors]
  y_pred = predict(model,x_test)
  if (type=="mape"){
    error = mape(y_test, y_pred) # rmse, mse .. 
    print(paste("Mean Absolute Percentage Error is", 100*error, "%"))
    error
  }
  else{
    print("Not implemented")
  }
}

#############################################################################
# Split train and test
#############################################################################

split = train_test_split(df)
train = split$train
test = split$test

#############################################################################
# UNIDIMENSIONAL REGRESSION
#############################################################################
# source http://users.stat.umn.edu/~helwig/notes/smooth-notes.html#example-1-prestige-from-income

# Local Regression
loess.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  tune.loess <- function(s){
    lo <- loess(y ~ x, span = s)
    mean((lo$fitted - y)^2) / (1 - lo$trace.hat/nobs)^2
  }
  os <- optimize(tune.loess, interval = c(.01, 99))$minimum
  lo <- loess(y ~ x, span = os)
  list(x = x, y = lo$fitted, df = lo$trace.hat, span = os)
}
# Kernel Regression
ksmooth.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  xdif <- outer(x, x, FUN = "-")
  tune.ksmooth <- function(h){
    xden <- dnorm(xdif / h)
    xden <- xden / rowSums(xden)
    df <- sum(diag(xden))
    fit <- xden %*% y
    mean((fit - y)^2) / (1 - df/nobs)^2
  }
  xrng <- diff(range(x))
  oh <- optimize(tune.ksmooth, interval = c(xrng/nobs, xrng))$minimum
  if(any(oh == c(xrng/nobs, xrng)))
    warning("Minimum found on boundary of search range.\nYou should retune model with expanded range.")
  xden <- dnorm(xdif / oh)
  xden <- xden / rowSums(xden)
  df <- sum(diag(xden))
  fit <- xden %*% y
  list(x = x, y = fit, df = df, h = oh)
}


df = sample_n(df, 1000)  # Sample some rows, to reduce computational time

# Columns we want to try
cols = c("bedrooms","bathrooms","bedfloors_ratio","bathfloors_ratio","floors","view","geodist_index",           
 "condition","grade","yr_old","renovate_index","ord_date",     
 "log10.sqm_living.","log10.sqm_lot.","log10.sqm_living15.","log10.sqm_lot15.")

for (i in 1:length(cols)){
  col = cols[i]
  # local averaging (cv span selection)
  locavg <- with(df, supsmu(as.double(unlist(df[col])), as.double(unlist(df["log10.price."])))) #length.out=100
  # local regression (gcv span selection)
  locreg = with(df, loess.gcv(as.double(unlist(df[col])), as.double(unlist(df["log10.price."])))) #length.out=100
  locreg$df
  # kernel regression (gcv span selection)
  kern = with(df, ksmooth.gcv(as.double(unlist(df[col])), as.double(unlist(df["log10.price."])))) #length.out=100
  kern$df
  # plot data
  x11()
  plot(as.double(unlist(df[col])), as.double(unlist(df["log10.price."])), xlab = col, ylab = "log10price")
  # add fit lines
  lines(locavg$x, locavg$y, lwd = 2)
  lines(locreg$x, locreg$y, lwd = 2, lty = 2, col = "red")
  lines(kern$x, kern$y, lwd = 2, lty = 3, col = "blue")
  # add legend
  legend("bottomright", c("supsmu", "loess", "ksmooth"),
         lty = 1:3, lwd = 2, col = c("black", "red", "blue"), bty = "n")
}
dev.off()


################################################################################
# MULTIVARIATE KERNEL REGRESSION
################################################################################
library(np)
library(dplyr)
# source https://bookdown.org/egarpor/NP-UC3M/kre-ii-multmix.html
df = read.csv("kc_cleaned.csv")
df_red = sample_n(df, 8000)   # Extremely heavy, computationally
df_red = df_red[c(cols,"log10.price.","has_ren","has_bas","waterfront")]
attach(df_red)
# Model with all variables (to be reduced)
#bw_wine <- np::npregbw(formula = log10.price. ~
#                         ordered(bedrooms)+ordered(bathrooms)+ordered(bedfloors_ratio)
#                       +ordered(bathfloors_ratio)+ordered(floors)+factor(waterfront)+ordered(view)+geodist_index+
#                       ordered(condition)+grade+yr_old+renovate_index+factor(has_ren)+factor(has_bas)+
#                         ord_date+log10.sqm_living.+log10.sqm_lot.+log10.sqm_living15.+log10.sqm_lot15., 
#                       data=df_red, 
#                       regtype = "lc")
# We can also use xdat, ydat to set regressors and response

# Bandwidth selection☺
bw_wine <- np::npregbw(formula = log10.price. ~ grade + log10.sqm_living.+geodist_index+renovate_index,
                       data=df_red, 
                       regtype = "ll",
                       ckerorder = 2,
                       ckertype = "gaussian" )
# ckerorder: kernel order 2 (default),4,6,8 
# ckertype: gaussian (default), epanechnikov, or uniform
# Regr Type: ll = Local-Linear, lc = Local-Constant
summary(bw_wine)

# Regression
fit_wine <- np::npreg(bw_wine)
summary(fit_wine)

# Plot the "marginal effects of each predictor" on the response
par(mar=c(1,1,1,1))
plot(fit_wine, plot.behavior = "plot-data")

#####################################################################################
# PROVE
#####################################################################################
# idea variables with few values could modelled with Poisson, binomial 
# and negative binomial regression. See if this is possible with a gam

#prova covMCD-bad - Too heavy
library(robustbase)

keep = c("grade","yr_old","renovate_index","log10.price.","log10.sqm_living.","log10.sqm_lot.","log10.sqm_living15."
         ,"log10.sqm_lot15.","geodist_index")

df_red = sample_n(df, 2000)
keep = c("geodist_index","log10.price.")
fit_MCD <- covMcd(x = df_red[,keep], alpha = .95, nsamp = "deterministic")  #alpha, adetermines the size of the subsets used for parameter estimation
plot(fit_MCD,classic=TRUE)
ind_best_subset <- fit_MCD$best
N <- nrow(df_red); p <- ncol(df)
dev.off()
par(mar=c(1,1,1,1))
x11()
plot(df_red, col=ifelse(1:N%in%ind_best_subset,"black","red"),pch=19)

#PROVA PCA

tourists = df[c(4:10,14,15,18,19,22:26,28:34)]
tourists = df[c(4:26,28:34)]

tourists.sd <- scale(tourists) #+++
tourists.sd <- data.frame(tourists.sd)

head(tourists.sd)

pc.tourists <- princomp(tourists.sd, scores=T)
pc.tourists
summary(pc.tourists)

# To obtain the rows of the summary: 
pc.tourists$sd  # standard deviation of the components
pc.tourists$sd^2/sum(pc.tourists$sd^2) # proportion of variance explained by each PC
cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2) # cumulative proportion of explained variance

# loadings (recall: coefficients of the linear combination of the original variables that defines each principal component)
load.tour <- pc.tourists$loadings #in ogni colonna vedi per cosa moltiplicare le features per ottenere la componente i (una PC per colonna)
x11() # graphical representation of the loadings of the first six principal components! bello
par(mfcol = c(2,2))
for(i in 1:4) barplot(load.tour[,i], ylim = c(-1, 1), main=paste("PC",i))


# Explained variance plots!
x11()
layout(matrix(c(2,3,1,3),2,byrow=T))
plot(pc.tourists, las=2, main='Principal components', ylim=c(0,4.5e7))
barplot(sapply(tourists,sd)^2, las=2, main='Original Variables', ylim=c(0,4.5e7), ylab='Variances')
plot(cumsum(pc.tourists$sd^2)/sum(pc.tourists$sd^2), type='b', axes=F, xlab='number of components', 
     ylab='contribution to the total variance', ylim=c(0,1))
abline(h=1, col='blue')
abline(h=0.8, lty=2, col='blue')
box()
axis(2,at=0:10/10,labels=0:10/10)
axis(1,at=1:ncol(tourists),labels=1:ncol(tourists),las=2)

# scores
scores.tourists <- pc.tourists$scores
x11()
plot(scores.tourists[,1:2])
abline(h=0, v=0, lty=2, col='grey') 

#confronto boxplot var iniziali vs PCs
x11()
layout(matrix(c(1,2),2))
boxplot(tourists.sd, las=2, col='gold', main='Standardized variables')
scores.tourists <- data.frame(scores.tourists)
boxplot(scores.tourists, las=2, col='gold', main='Principal components') 

#quel grafico complicato con tutte le freccette
x11()
biplot(pc.tourists) 

mod1 = lm(df$log10.price.~., data=tourists.sd)
summary(mod1)
mod2 = lm(df$log10.price.~., data=scores.tourists[,1:13])
summary(mod2)

library(MASS)
library(robustbase)
attach(tourists)
fit_lms <- lmsreg(df$log10.price.~., data=tourists)	#least median squares
plot(df$log10.price.)
abline(fit_lms, col="red", lwd=2)
plot(fit_lms)
summary(fit_lms.coefficients)
fit_lts <- ltsReg(df$log10.price.~., data=tourists.sd, alpha=.95,mcd=TRUE)	#least trimmed squares
plot(df$log10.price.)
abline(fit_lts, col="red", lwd=2)
plot(fit_lts)


eval_regr(fit_lms, test, "mape")



#Model selection - Ignorant approach
library(caret)
library(leaps)
train.control <- trainControl(method = "cv", number = 10)
step.model <- train(y_train~.,data=cbind(X1,y_train),method = "leapSeq", 
                    tuneGrid = data.frame(nvmax = 1:11),
                    trControl = train.control) 
step.model$results
step.model$bestTune
summary(step.model$finalModel)