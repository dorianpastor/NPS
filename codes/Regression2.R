library(dplyr)
library(MASS)
df = read.csv("kc_cleaned.csv")
attach(df)
# "id"  "date"  "price" "zipcode" "lat" "long"  "yr_built"  "yr_renovated"
# "sqm_living"   "sqm_lot"   "sqm_above"   "sqm_basement"   "sqm_living15"   "sqm_lot15" 
# "bedrooms"  "bathrooms" "bedfloors_ratio" bathfloors_ratio" "floors"  "waterfront"  "view" "geodist_index"           
# "condition" "grade" "yr_old"  "renovate_index"  "has_ren" "has_bas" "ord_date"
# "log10.sqm_living."  "log10.sqm_lot."  "log10.sqm_living15."  "log10.sqm_lot15."
# "log10.price." 

# Useless: "id", "date", "yr_built", "yr_renov", "zipcode", "lat", "long", "price"
# We use the logs of "sqm_living", "sqm_lot", "sqm_above", "sqm_basement" instead of the originals

# Target: "log10.price."


#############################################################################
# UNIDIMENSIONAL
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
# source https://bookdown.org/egarpor/NP-UC3M/kre-ii-multmix.html
df = read.csv("kc_cleaned.csv")
df_red = sample_n(df, 10)   # Extremely heavy, computationally
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
plot(fit_wine)

#####################################################################################
# idea variables with few values could modelled with Poisson, binomial 
# and negative binomial regression. See if this is possible with a gam
