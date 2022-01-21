library(ISLR2)
library(car)
library(np)
library(splines)
library(fda)

df = read.csv("kc_cleaned.csv")
df = df[-which(df$bedrooms==0),]
attach(df)

resp = log10.price.

################################################################################
cov = log10.sqm_living.
################################################################################
range(cov)
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))


#SEMBRA OK
m_locl01 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 0.1)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locl01,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Log ( surface )",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)


#PESSIMO
m_locl02 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 0.2)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locl02,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Log ( surface )",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

#VEDIAMO.. Così così.
m_locl03 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 0.05)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locl03,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Log ( surface )",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# Il migiore din-din-din
m_locl04 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 0.075)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locl04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Log ( surface )",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)


################################################################################
cov = geodist_index
################################################################################
range(cov)
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))

#BUONO BUONO
m_locg01 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 5)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg01,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# Troppo poco smooth, specialmente alla fine. Da scartare.
m_locg02 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 3)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg02,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# Forse troppo poco smooth... Sì, da scartare.
m_locg03 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 7)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg03,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# Forse..
m_locg04 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 6)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

par(mfrow = c(2,2))
preds=predict(m_locg04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg02,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg01,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg03,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

#Let's see 
m_locg05 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 4)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg05,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

#
m_locg06 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 5.5)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg06,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# E' leiiii!!!
m_locg07 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 4.5)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg06,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

par(mfrow = c(2,2))
preds=predict(m_locg05,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg05,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg01,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

preds=predict(m_locg07,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)


################################################################################
cov = grade
################################################################################
range(cov)
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))

# Fa un po' schifo da mettere nella presentazione..
m_locgr01 = npreg(resp ~ cov, ckertype = 'gaussian', bws = 1.5)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locgr01,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Grade",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

#già meglio, però...
m_locgr02 = npreg(resp ~ cov, ckertype = 'gaussian', bws = .75)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locgr02,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Grade",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# non pessimo..
m_locgr03 = npreg(resp ~ cov, ckertype = 'gaussian', bws = .5)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locgr03,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Grade",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

# ok...
m_locgr04 = npreg(resp ~ cov, ckertype = 'gaussian', bws = .625)
# … : “gaussian”, “truncated gaussian”, “epanechnikov”, “uniform”, # … : bandwidth (dipende da cov)
#cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locgr04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ", main = 'Gaussian kernel',xlab = "Grade",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)


################################################################################
###########################  P  L  O  T     ####################################
################################################################################

par(mfrow=c(3,1))

cov = log10.sqm_living.
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locl04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ",xlab = "Log ( surface )",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

cov = geodist_index
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locg06,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ",xlab = "Distance from center",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)

cov = grade
cov_grid = data.frame(cov= seq(range(cov)[1],range(cov)[2],length.out=100))
preds=predict(m_locgr04,newdata=cov_grid, se=T)
se.bands=cbind(preds$fit +2* preds$se.fit ,preds$fit -2* preds$se.fit)
plot(cov,resp, xlim = range(cov_grid),cex = .5, col = " darkgrey ",xlab = "Grade",ylab = "Log ( price )")
lines(cov_grid$cov,preds$fit ,lwd =2, col =" black")
matlines(cov_grid$cov,se.bands ,lwd =1, col =" black",lty =3)
