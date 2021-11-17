library(dplyr)
df = read.table('house_data_1511.txt', header=TRUE, sep=' ')
attach(df)

keep = c("sqm_above", "sqm_basement", "sqm_living", "sqm_living15", "sqm_lot",
         "sqm_lot15", "yr_old", "renovate_index", "geo_dist") # grade looks like a parabola

par(mfrow=c(3,3))
par(mar=c(1,1,1,1))
for (i in keep){
  colnum <- which(colnames(df) %in% i)
  plot(df[,colnum], price,  main = i)
}

# TODO: compute param polynomial regr


# Univariate against price to think of a step as a consequence
# Note: VERY evident outliers 7253, 3915, 9255 
View(df[c(7253,3915,9255),])
df = df[-c(7253,3915,9255),]
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


# TODO: Polynomial step regression, set a step at the points
# where variance increases and normality goes bad
# from lectures try: Kernel smoothing, Splines,Mercer Kernels