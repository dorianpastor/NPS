library(dplyr)
df = read.table('house_data_1511.txt', header=TRUE, sep=' ')
# Note: VERY evident outliers 7253, 3915, 9255 
df = df[-c(7253,3915,9255),]
attach(df)

# TODO: Polynomial step regression (also parametric as comparison), set a step at the points
# where variance increases and normality goes bad
# from lectures try: Kernel smoothing, Mercer Kernels
# finish with Splines

# Regression ideas: log price (see sqm_living, sqm_above), sqm lot should be at denominator (see regr plot)
# geo_dist is a good place where to apply a step?

keep = c("sqm_above", "sqm_basement", "sqm_living", "sqm_living15", "sqm_lot",
         "sqm_lot15", "yr_old", "renovate_index", "geo_dist") # grade looks like a parabola

par(mfrow=c(3,3))
par(mar=c(1,1,1,1))
for (i in keep){
  colnum <- which(colnames(df) %in% i)
  plot(df[,colnum],price,  main = i)
}

# Univariate against price to think of a step as a consequence
#--------------------------------------------------------------------------
mod = lm(price~sqm_above)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
# non-linear relationships, no step 
par(mfrow=c(1,1))
with(df,plot(sqm_above, price))
abline(mod,col="red")
#--------------------------------------------------------------------------
mod = lm(price~sqm_basement)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
# non-linear relationships, no step 
par(mfrow=c(1,1))
with(df,plot(sqm_basement, price))
abline(mod,col="red")
#--------------------------------------------------------------------------
mod = lm(price~sqm_living)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))
with(df,plot(sqm_living, price))
abline(mod,col="red")
#--------------------------------------------------------------------------
mod = lm(price~sqm_lot)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
# non-linear relationships
par(mfrow=c(1,1))
with(df,plot(sqm_lot, price))
abline(mod,col="red")  # should be an hyperbole, not a line, lot should go to denominator
#---------------------------------------------------------------------------
# So far we observed a drift with the increase of the sizes of the house, the linear model
# can't explain large houses. The variance also increases with larger houses
#--------------------------------------------------------------------------
mod = lm(price~yr_old)
summary(mod)
par(mfrow=c(2,2))
plot(mod)
# very bad model
par(mfrow=c(1,1))
with(df,plot(yr_old, price))
abline(mod,col="red")
#--------------------------------------------------------------------------
mod = lm(price~renovate_index)
summary(mod)
x11()
par(mfrow=c(2,2))
plot(mod)
dev.off()
# very bad model
par(mfrow=c(1,1))
with(df,plot(renovate_index, price))
abline(mod,col="red")
#--------------------------------------------------------------------------
mod = lm(price~geo_dist)
summary(mod)
x11()
par(mfrow=c(2,2))
plot(mod)
dev.off()
# very bad model, we are not capturing the pattern at high geo_dist
par(mfrow=c(1,1))
with(df,plot(geo_dist, price))
abline(mod,col="red")


#################################################################
# from lab10
# STEP REGR - cut(income,breaks = c(min(income),10000,max(income)))
# codice ancora non funzionante. Tentativo di mettere 2 breaks e vedere cosa salta fuori
#--------------------------------------------------------------------------
#i = 2
vars = keep#[i]
#par(mfrow=c(3,3))
#par(mar=c(1,1,1,1))
par(mfrow=c(1,1))
for (i in 1:length(vars)){
  var = as.numeric(unlist(df[vars[i]]))
  m_cut = lm(price~cut(var, breaks=2))
  summary(m_cut)
  var.grid=with(df, seq(range(var)[1],range(var)[2],by=10))
  preds=predict(m_cut,list(var=var.grid),se=T)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  with(df, plot(var ,price ,xlim=range(var.grid) ,cex =.5, col =" darkgrey ",main='Custom cut Fit'))
  #jpeg(paste0("myplot",keep[i],".jpg"))
  lines(var.grid,preds$fit ,lwd =2, col =" blue")
  matlines(var.grid ,se.bands ,lwd =1, col =" blue",lty =3)
  #dev.off()
}




#################################################################
# from lab11
library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)

vars = keep
FRAC = 5 # it means I use a N/FRAC of the total points for the local averaging
DF = 50 # # Degrees of freedom for smoothing spline, the smaller the lesser overfitting

# sqm_above and sqm_basement not so great but CV better than GCV, sqm_living sucks, sqm_living15 good
# sqm_lot no, sqm_lot15 skipped, yr_old ok, renovate_index ok, geo_dist good but slightly overfitting
i = 2  # CHANGE THE INDEX TO MAKE REGR. ON THE OTHER VARS
var = as.numeric(unlist(df[vars[i]]))
m_loc = npreg(price ~ var,
              ckertype = 'gaussian', # try "gaussian"
              bws = (range(var)[2]-range(var)[1])/FRAC, # bandwidth #needs to be adjusted
              data = df)

var_newdata=data.frame(var=with(df, seq(range(var)[1],range(var)[2],by=0.5)))
preds=predict(m_loc,newdata=var_newdata,se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
with(df,plot(var,price,xlim = range(var_newdata$var),cex = .5,col = " darkgrey ",
    main = paste0('Local Averaging\n','Uniform kernel\n',keep[i])))
lines(var_newdata$var,preds$fit ,lwd =2, col =" blue")
matlines(var_newdata$var,se.bands ,lwd =1, col =" blue",lty =3)
# HERE smoothing spline -> seems pretty good for sqm_lot, not sqm_lot15
# With hyperparameter tuning
fit_smooth_spline_CV <- with(df, smooth.spline(var,price,cv = TRUE))
fit_smooth_spline_GCV <- with(df, smooth.spline(var,price,cv = FALSE))
with(Prestige, plot(var,price, cex =.5, col =" darkgrey "))
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1)
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2)
legend(20000, 30, legend=c("CV", "GCV"),col=c("red", "blue"), lty=1:2, cex=0.8)



# TRY PIECEWISE LINEAR, QUADRATIC, CUBIC AND B-SPLINES







#####
#other stuff
# smoothing spline with With hand-tuning
#fit_smooth_spline <- with(df, smooth.spline(var,price,df=DF))  # can also specify smoothing par: lambda = 1e-1
#with(df, plot(var ,price, cex =.5, col =" darkgrey "))
#lines(fit_smooth_spline,col="blue",lwd=2)