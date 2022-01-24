library(car)
library(np)
library(ISLR2)
library(car)
library(mgcv)
library(rgl)
library(splines)
library(pbapply)

# Alcune funzioni vengono prese da "Regression_final.R"
# In particolare, runnare:
# - target (linea 11)
# - train_test_split (linea 17)
# - select_columns (linea 26)
# - eval_regr (linea 30)
# - useful_gen (linea 74)
# - useful_geo (linea 78)
# - useful_sqm (linea 78)

attach(kc_cleaned)
dfnew = kc_cleaned[which(price>=1000000),]
nnew = dim(dfnew)[1]


plot_regr_2d <- function(x, y, model, xname, option="",cutoff=0){
  x.grid=seq(range(x)[1],range(x)[2],length.out=1000)
  newdata = data.frame(x.grid)
  if (option=="linstep")
  {newdata$cutl = (x.grid-cutoff)*(x.grid>cutoff)
  if (length(labels(terms(model)))==3) newdata$bo = x.grid>cutoff
  colnames(newdata) = labels(terms(model))}
  else
    colnames(newdata) = xname
  preds=predict(model,newdata,se=T, length.out=1000)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  plot(x, y, xlim=range(x.grid), xlab=xname, ylab=target, pch=20, cex =.5, col =" darkgrey ",main='Linear Fit')
  lines(x.grid,preds$fit ,lwd =2, col =" blue")
  matlines (x.grid ,se.bands ,lwd =1, col =" blue",lty =3)
}

split = train_test_split(dfnew)
x_train = split$x_train; y_train = split$y_train
x_test = split$x_test; y_test = split$y_test;

attach(x_train)

###################################################
# Rejected
plot(bedfloors_ratio,y_train)
plot(ord_date,y_train)
plot(condition,y_train)

###################################################
# "bathfloors_ratio"

###################################################
col = "bathfloors_ratio"
plot(data.frame(x_train[col], y_train), pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)

# Option 0 - cubic spline
model = gam(y_train ~ s(bathfloors_ratio, bs="cr"))
plot(model)
plot_regr_2d(bathfloors_ratio, y_train, model, col)
summary(model)
# Option 1 - natural spline 2 - BEST (degree 2 because it's monotone)
model_gam_ns = lm(y_train ~ ns(bathfloors_ratio, df=2)) # note: != poly(degree=2)
plot_regr_2d(bathfloors_ratio, y_train, model_gam_ns, col)
summary(model_gam_ns)

# Option 2 - step
model_cut = lm(y_train ~ cut(bathfloors_ratio, 
                             breaks = c(min(bathfloors_ratio),2,max(bathfloors_ratio))))
plot_regr_2d(bathfloors_ratio, y_train, model_cut, col)
summary(model_cut)
# Option 3 - linstep
cutl <- (bathfloors_ratio-2)*(bathfloors_ratio>2)
model_cut2 = lm(y_train ~ cutl + bathfloors_ratio)
plot_regr_2d(bathfloors_ratio, y_train, model_cut2, col, "linstep")


###################################################
# "view"
###################################################
col = "view"
plot(data.frame(x_train[col], y_train), pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train~view)
plot(model)
plot_regr_2d(view, y_train, model, col, "linstep")
summary(model)

# tentativo #2: poca roba
model_gam_ns = lm(y_train ~ ns(view, df=5)) # note: != poly(degree=2)
plot_regr_2d(view, y_train, model_gam_ns, col)
summary(model_gam_ns)


###################################################
# "condition"
###################################################
col = "condition"
plot(data.frame(x_train[col], y_train), pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 1
model_cut = lm(y_train ~ cut(condition, 
                             breaks = c(min(condition),3,max(condition)),
                             include.lowest = T, right=F))
summary(model_cut)
plot_regr_2d(condition, y_train, model_cut, col)

# Option 2
cutoff = 1
cutl <- (condition-cutoff)*(condition>cutoff)
model_cut2 = lm(y_train ~ condition + cutl + I(condition>cutoff))
summary(model_cut2)
plot_regr_2d(condition, y_train, model_cut2, col, "linstep", cutoff)  
# Comment: condition alone is not good at 0, I tried all combinations with cutoff=1
# flat+linear(2nd best - using only I()), linear+linear, flat+flat(best).

# tentativo 3:meglio ma evitabile nel modello
model_j = lm(y_train ~ condition:I(condition>3))
summary(model_j)
plot_regr_2d(condition, y_train, model_j, col)  


###################################################
# "grade"
###################################################
col = "grade"
plot(data.frame(x_train[col], y_train), pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train~grade)
plot(model)
summary(model)

# tentativo 2: niente di che
model_gam_ns = lm(y_train ~ ns(grade, df=1)) # note: != poly(degree=2)
plot_regr_2d(grade, y_train, model_gam_ns, col)
summary(model_gam_ns)

###################################################
# "renovate_index"
###################################################
col = "renovate_index"
plot(data.frame(x_train[col], y_train), pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 1
model_gam_ns = lm(y_train ~ ns(renovate_index, df=3))
plot_regr_2d(renovate_index, y_train, model_gam_ns, col)
summary(model_gam_ns) # R^2_adj pessimo

# Option 2
cutoff = 80
cutl <- (renovate_index-cutoff)*(renovate_index>cutoff)
model_cut2 = lm(y_train ~ renovate_index + cutl ) #+ I(renovate_index>cutoff)
summary(model_cut2)
plot_regr_2d(renovate_index, y_train, model_cut2, col, "linstep", cutoff)  
# Option 3

model = lm(y_train~I(renovate_index<cutoff))
summary(model)

# Option 4 - repeat the previous ones, but only for non-renovated houses -> no relevant changes on the right tail
renovate_index_ = x_train[which(has_ren==0),col]; y_train_ = y_train[which(has_ren==0)]
plot(data.frame(renovate_index_, y_train_), pch=20)
model_cut2 = lm(y_train_ ~ renovate_index_ + I((renovate_index_-cutoff)*(renovate_index_>cutoff) )) 
plot_regr_2d(renovate_index, y_train, model_cut2, col, "linstep", cutoff)  
model = lm(y_train_~I(renovate_index_<cutoff))
summary(model)
# None of them is convincing, or we acknowledge that when they get old they gain value
# Note: we decided to have 80 as cutoff; only 4% houses were renovated

# tentativo 5:
model_j = lm(y_train~renovate_index)
summary(model_j)

###################################################
# c("has_ren", "yr_old")
###################################################
col = "yr_old"
plot(data.frame(x_train[col], y_train),pch=20)
points(data.frame(x_train[which(has_ren==1),col], y_train[which(has_ren==1)]), col="red",pch=20)
legend("topright", c("no_renovation", "renovated"),
       lty = 1:2, lwd = 2, col = c("black", "red"), cex=0.75)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)

# Option 1
model = lm(y_train ~ has_ren*yr_old)
summary(model)

# Option 2 (slightly higher R2)
model2 = lm(y_train ~ has_ren*renovate_index)
summary(model2)
# Option 3 - BEST, we model the age of the house and add a bonus to the renovated
model3 = lm(y_train ~ I((yr_old-80)*(yr_old>80))
            + yr_old:has_ren)
summary(model3)

###################################################
# "geodist_index"
###################################################
col = "geodist_index"
plot(data.frame(x_train[col], y_train),pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 1 - natural spline 2 - BEST (degree 2 because it's monotone)
model_gam_ns = lm(y_train ~ bs(geodist_index, degree=3)) # note: != poly(degree=2)
plot_regr_2d(geodist_index, y_train, model_gam_ns, col)
summary(model_gam_ns)

# Option 2 - good
cutoff = 40  # note: only 3% houses have > 40
cutl <- (geodist_index-cutoff)*(geodist_index>cutoff)
model_cut2 = lm(y_train ~ geodist_index+cutl)
summary(model_cut2)
plot_regr_2d(geodist_index, y_train, model_cut2, col, "linstep", cutoff) 

# Option 3 - good, equivalent
model = lm(y_train ~ bs(geodist_index, degree=2))
summary(model)
plot_regr_2d(geodist_index, y_train, model, col)

###################################################
# "log10.sqm_living."
###################################################
col = "log10.sqm_living."
plot(data.frame(x_train[col], y_train),pch=20)
# scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_living.)
plot(model)
summary(model)
plot_regr_2d(log10.sqm_living., y_train, model, col) 

# tentativo 2
model_gam_ns = lm(y_train ~ ns(log10.sqm_living., df=3)) 
summary(model_gam_ns)
plot_regr_2d(log10.sqm_living., y_train, model_gam_ns, col) 

# tentativo 3
model = lm(y_train ~ bs(log10.sqm_living., degree=2))
summary(model)
plot_regr_2d(log10.sqm_living., y_train, model, col) 

###################################################
# "log10.sqm_lot."
###################################################
col = "log10.sqm_lot."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_lot.)
plot_regr_2d(log10.sqm_lot., y_train, model, col) 
summary(model)
plot(model)

# tentativo 2
model_gam_ns = lm(y_train ~ ns(log10.sqm_lot., df=3)) 
summary(model_gam_ns)
plot_regr_2d(log10.sqm_lot., y_train, model_gam_ns, col) 

# tentativo 3
model = lm(y_train ~ bs(log10.sqm_lot., degree=2))
summary(model)
plot_regr_2d(log10.sqm_lot., y_train, model, col)

###################################################
# "log10.sqm_living15."
###################################################
col = "log10.sqm_living15."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_living15.)
summary(model)
plot(model)

# tentativo 2
model_gam_ns = lm(y_train ~ ns(log10.sqm_living15., df=2)) 
summary(model_gam_ns)
plot_regr_2d(log10.sqm_living15., y_train, model_gam_ns, col) 

# tentativo 3
model = lm(y_train ~ bs(log10.sqm_living15., degree=2))
summary(model)
plot_regr_2d(log10.sqm_living15., y_train, model, col)

###################################################
# "log10.sqm_lot15."
###################################################
col = "log10.sqm_lot15."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_lot15.)
summary(model)
plot(model)

# tentativo 2
model_gam_ns = lm(y_train ~ ns(log10.sqm_lot15., df=2)) 
summary(model_gam_ns)
plot_regr_2d(log10.sqm_lot15., y_train, model_gam_ns, col) 

# tentativo 3
model = lm(y_train ~ bs(log10.sqm_lot15., degree=2))
summary(model)
plot_regr_2d(log10.sqm_lot15., y_train, model, col)

###################################################
# Final model
###################################################
selector = select_columns(useful_gen[c(-1,-3,-5)],useful_geo,useful_sqm)
X1 = selector$x_train; x_test_1 = selector$x_test
model_final = lmrob(y_train~ns(bathfloors_ratio, df=2)+ # non è possibile inserire la versione gam?
                                                        # 0.02079 o 0.0278
                      view +                            # 0.03851 
                      grade +                           # 0.08844
                  #   condition:I(condition>3) +        # 0.001935 
                  #   I((yr_old-80)*(yr_old>80)) +      # letteralmente useless
                  #    yr_old:has_ren + 
                      bs(geodist_index, degree=2) +     # 0.03114
                      bs(log10.sqm_living., degree=2) + # 0.1429
                      bs(log10.sqm_lot., degree=2) +    # 0.04253
                      log10.sqm_living15.+              # 0.0561 
                      ns(log10.sqm_lot15., df=2)        # 0.04307
                     , data=X1
)  # Using lmrob's MM-type estimator for lm
summary(model_final)
eval_regr(model_final,x_test_1,y_test, "mae") # Ignore warning (it's because the test data has geodists that are lower than the minimum found in the train data)
eval_regr(model_final,x_test_1,y_test, "mape")
