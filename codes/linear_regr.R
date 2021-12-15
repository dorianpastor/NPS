library(dplyr)
library(MASS)
df = read.csv("kc_cleaned.csv")
attach(df)
fm= lm(log10.price.~ bathrooms + bedrooms + bathfloors_ratio+ bedfloors_ratio + floors + waterfront + 
         view + geodist_index + condition + grade + yr_old + renovate_index + log10.sqm_living. + 
         log10.sqm_living15. + log10.sqm_lot. + log10.sqm_lot15.)
summary(fm)
#remove log10.sqm_lot15.
fm=lm(log10.price.~ bathrooms + bedrooms + bathfloors_ratio+ bedfloors_ratio + floors + waterfront + 
         view + geodist_index + condition + grade + yr_old + renovate_index + log10.sqm_living. + 
         log10.sqm_living15. + log10.sqm_lot.)
summary(fm)
plot(fm$residuals)
qqnorm(scale(residuals(fm)))
abline(0,1)
#pessimo qqplot, non si può assumere normalità dei residui