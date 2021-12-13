setwd("C:/Users/Messoanuovo.it/Desktop/Progetto NPS/House")
getwd()
DATA<-read.csv("kc_cleaned.csv", header=TRUE, sep=",")
attach(DATA)

df = DATA

keep = c("sqm_above", "sqm_basement", "sqm_living", "sqm_living15", "sqm_lot",
         "sqm_lot15", "yr_old", "renovate_index", "geodist_index", "bathfloors_ratio", "waterfront", "floors","condition") # grade looks like a parabola

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
vars = keep #[i]
#par(mfrow=c(3,3))
#par(mar=c(1,1,1,1))
par(mfrow=c(1,1))

  i = 1
  var = as.numeric(unlist(df[vars[i]]))
  plot(var,log10.price.)
  plot(var, price)
  m_cut = lm(log10.price.~cut(var, breaks=3))
  summary(m_cut)
  var.grid=with(df, seq(range(var)[1],range(var)[2],length.out=100))
  preds=predict(m_cut,list(var=var.grid),se=T)
  se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
  with(df, plot(var ,log10.price. ,xlim=range(var.grid) ,cex =.5, col =" darkgrey ",main='Custom cut Fit'))
  #jpeg(paste0("myplot",keep[i],".jpg"))
  lines(var.grid,preds$fit ,lwd =2, col =" blue")
  matlines(var.grid ,se.bands ,lwd =1, col =" blue",lty =3)
  #dev.off()




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

var_newdata=data.frame(var=with(df, seq(range(var)[1],range(var)[2],length.out=100)))
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






#Spline:
df = DATA
attach(DATA)
keep

resp = log10.price.
cov = renovate_index
plot(cov,resp)
model_spline <- lm(resp ~ bs(cov, knots=100, degree=3)) # . : degree delle spline (magari 3)
#  model_spline <- lm(resp ~ bs(cov, ,degree=., df = .)) # . : df = degree + #knots.
cov_grid = seq(range(cov)[1],range(cov)[2], length.out=100)
preds=predict(model_spline,list(cov=cov_grid),se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim=range(cov) ,cex =.5, col =" darkgrey " )
lines(cov_grid,preds$fit ,lwd =2, col =" blue")
matlines(cov_grid, se.bands ,lwd =1, col =" blue",lty =3)
summary(model_spline)
#plot(model_spline)

#Natural cubic spline:
knots <- quantile(cov,probs=c(0.1,0.5, 0.9)) 
boundary_knots <- quantile(cov,probs=c(0.05,0.95))
model_ns=lm(resp ~ ns(cov,knots=knots,Boundary.knots=boundary_knots))
cov_grid = seq(range(cov)[1],range(cov)[2], length.out=100)
preds=predict(model_ns, list(cov=cov_grid),se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov ,resp ,xlim=range(cov) ,cex =.5, col =" darkgrey " )
lines(cov_grid,preds$fit ,lwd =2, col =" blue")
matlines(cov_grid, se.bands ,lwd =1, col =" blue",lty =3)
summary(model_ns)
#plot(model_ns)

#Smoothing spline:

fit_smooth_spline <- smooth.spline(cov,resp,df=20)
# fit_smooth_spline <- smooth.spline(cov,resp,lambda = 0.01)
plot(cov ,resp, cex =.5, col =" darkgrey ")
lines(fit_smooth_spline,col="blue",lwd=2)

fit_smooth_spline_CV <-smooth.spline(cov,resp,cv = TRUE)
fit_smooth_spline_GCV <- smooth.spline(cov,resp,cv = FALSE)
plot(cov,resp, cex =.5, col =" darkgrey ")
lines(fit_smooth_spline_CV,col="red",lwd=2,lty=1)
lines(fit_smooth_spline_GCV,col="blue",lwd=2, lty=2)
legend("topleft ",legend=c("CV", "GCV"),col=c("red", "blue"), lty=1:2, cex=0.8)

# a parte
mod = lm(resp ~ log(sqm_living,10) + log(sqm_above,10))
summary(mod)

#####
#other stuff
# smoothing spline with With hand-tuning
#fit_smooth_spline <- with(df, smooth.spline(var,price,df=DF))  # can also specify smoothing par: lambda = 1e-1
#with(df, plot(var ,price, cex =.5, col =" darkgrey "))
#lines(fit_smooth_spline,col="blue",lwd=2)