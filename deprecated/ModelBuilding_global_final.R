library(car)

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

attach(x_train)

###################################################
# Rejected
scatterplot(bedfloors_ratio,y_train)
scatterplot(ord_date,y_train)
###################################################
# "bathfloors_ratio"
###################################################
col = "bathfloors_ratio"
plot(data.frame(x_train[col], y_train), pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 0 - cubic spline
model = gam(y_train ~ s(bathfloors_ratio, bs="cr"))
plot(model)
plot_regr_2d(bathfloors_ratio, y_train, model, col)
# Option 1 - natural spline 2 - BEST (degree 2 because it's monotone)
model_gam_ns = lm(y_train ~ ns(bathfloors_ratio, df=2)) # note: != poly(degree=2)
plot_regr_2d(bathfloors_ratio, y_train, model_gam_ns, col)
# Option 2 - step
model_cut = lm(y_train ~ cut(bathfloors_ratio, 
                                  breaks = c(min(bathfloors_ratio),2,max(bathfloors_ratio))))
plot_regr_2d(bathfloors_ratio, y_train, model_cut, col)
# Option 3 - linstep
cutl <- (bathfloors_ratio-2)*(bathfloors_ratio>2)
model_cut2 = lm(y_train ~ cutl + bathfloors_ratio)
plot_regr_2d(bathfloors_ratio, y_train, model_cut2, col, "linstep")                 
###################################################
# "view"
###################################################
col = "view"
plot(data.frame(x_train[col], y_train), pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train~view)
plot(model)
###################################################
# "condition"
###################################################
col = "condition"
plot(data.frame(x_train[col], y_train), pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
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
###################################################
# "grade"
###################################################
col = "grade"
plot(data.frame(x_train[col], y_train), pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train~grade)
plot(model)
###################################################
# "renovate_index"
###################################################
col = "renovate_index"
plot(data.frame(x_train[col], y_train), pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 1
model_gam_ns = lm(y_train ~ ns(renovate_index, df=3))
plot_regr_2d(renovate_index, y_train, model_gam_ns, col)
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
###################################################
# c("has_ren", "yr_old")
###################################################
col = "yr_old"
plot(data.frame(x_train[col], y_train),pch=20)
points(data.frame(x_train[which(has_ren==1),col], y_train[which(has_ren==1)]), col="red",pch=20)
legend("bottomleft", c("no_renovation", "renovated"),
       lty = 1:2, lwd = 2, col = c("black", "red"), cex=0.75)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
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
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
# Option 1 - natural spline 2 - BEST (degree 2 because it's monotone)
model_gam_ns = lm(y_train ~ bs(geodist_index, degree=3)) # note: != poly(degree=2)
plot_regr_2d(geodist_index, y_train, model_gam_ns, col)
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
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_living.)
plot(model)
###################################################
# "log10.sqm_lot."
###################################################
col = "log10.sqm_lot."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_lot.)
plot(model)
###################################################
# "log10.sqm_living15."
###################################################
col = "log10.sqm_living15."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_living15.)
summary(model)
plot(model)
###################################################
# "log10.sqm_lot15."
###################################################
col = "log10.sqm_lot15."
plot(data.frame(x_train[col], y_train),pch=20)
scatterplot(x=unlist(x_train[col]), y=y_train, xlab=col,ylab=target)
model = lm(y_train ~log10.sqm_lot15.)
summary(model)
plot(model)

###################################################
# Final model
###################################################
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
                   log10.sqm_lot15.
                 ,data=X1
                 )  # Using lmrob's MM-type estimator for lm
summary(model_final)
eval_regr(model_final,x_test_1,y_test, "mae") # Ignore warning (it's because the test data has geodists that are lower than the minimum found in the train data)
eval_regr(model_final,x_test_1,y_test, "mape")
