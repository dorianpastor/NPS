#setwd("C:/Users/panze/Desktop/NONPARAM STATISTICS/PROGETTO_GITHUB/NPS")

library(MASS)
library(rgl)
library(DepthProc)
library(gam)
library(plot3D)
library(lme4)
library(nlme)
library(hexbin)
library(aplpack)
library(robustbase)

D = read.csv("kc_cleaned.csv")
attach(D)
LogY = log10.price.
Log_sqm_living = log10.sqm_living.
Log_sqm_lot = log10.sqm_lot.
Log_sqm_living15 = log10.sqm_living15.
Log_sqm_lot15 = log10.sqm_lot15.

##########################################################################################
# Testing
##########################################################################################

####### Gaussian LogY test #############

B<-10000
shapiro.pvalue<-numeric(B)
for (b in 1:B){
  Yobs<-sample(LogY,5000)
  shapiro.pvalue[b]<-shapiro.test(Yobs)$p.value
}
sum(shapiro.pvalue>=0.1)/B  #0 
#refuse normality of LogY


######## H0: C(Log_sqm_living) == C(Log_sqm_living15) vs H1: C(Log_sqm_living) != C(Log_sqm_living15) ######################

x11()
par(mfrow=c(1,3))
plot(Log_sqm_living,main='occurencies in DATA') 
hist(Log_sqm_living,main = 'distribution of occurencies',prob=T)
boxplot(Log_sqm_living)

x11()
par(mfrow=c(1,3))
plot(Log_sqm_living15,main='occurencies in DATA') 
hist(Log_sqm_living15,main = 'distribution of occurencies',prob=T)
boxplot(Log_sqm_living15)

t1 <- cbind(Log_sqm_living)
t2 <- cbind(Log_sqm_living15)
p  <- dim(t1)[2]
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n <- n1+n2
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)
delta.0 <- 0
diff <- t1-t2
diff.mean <- colMeans(diff)

T20 <- as.numeric((diff.mean-delta.0)  %*% (diff.mean-delta.0))

T2 <- numeric(B)
for(perm in 1:B)
{
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

x11()
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)
x11()
plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)
p_val <- sum(T2>=T20)/B
p_val #0.324
#we don't reject H0
#I can argue that the Log_sqm_living is equal to the Log_sqm_living15
#So the dimensions of houses sold are equal to the dimensions of the 15 nearest houses
#for example: the house sold is, on average, in a set of terraced houses (villette a schiera)
#or in a set of villas

########## H0: D_OBBR$log10.price. =d= D_RBNR$log10.price. vs H1: D_OBBR$log10.price. =!d!= D_RBBR$log10.price. ######################

D_OBBR <- D[which(yr_built <= 1990 & yr_renovated != 0),]
D_RBNR <- D[which(yr_built > 1990 & yr_renovated == 0),]

T0<-abs(mean(D_OBBR$log10.price.)-mean(D_RBNR$log10.price.))
DataPooled<-c(D_OBBR$log10.price.,D_RBNR$log10.price.)
n<-length(DataPooled)
B<-10000
tstat<-numeric(B)
for(perm in 1:B){
  permutation<-sample(1:n)
  x_perm<-DataPooled[permutation]
  x1_perm<-x_perm[1:length(D_OBBR$log10.price.)]
  x2_perm<-x_perm[(length(D_RBNR$log10.price.)+1):n]
  tstat[perm]<-abs(mean(x1_perm)-mean(x2_perm))
}

x11()
hist(tstat,xlim=range(c(tstat,T0)),breaks = 20)
abline(v=T0,col='red')  
x11()
plot(ecdf(tstat))
pvalue<-sum(tstat>=T0)/B
pvalue #0
#we reject H0 
#we can say, with a level of confidence higher than 99%, that LogY in old built but renovated houses is significantly different from LogY in recently built not renovated houses


########### Verify effect of waterfront on LogY ###################

x11()
boxplot(LogY ~ waterfront, col=rainbow(8),main='Effect of waterfront on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

waterfront_fac <- factor(waterfront, levels=c('0','1'))
i1 <- which(waterfront_fac=='1')
i2 <- which(waterfront_fac=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(waterfront_fac))

fit <- aov(LogY ~ waterfront_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- LogY[permutation]
  fit_perm <- aov(LogY_perm ~ waterfront_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor waterfront has significantly effect on LogY


################# Verify effect of view on LogY ####################

x11()
boxplot(LogY ~ view, col=rainbow(8),main='Effect of view on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

view_fac <- factor(view, levels=c('0','1','2','3','4'))
i0 <- which(view_fac=='0')
i1 <- which(view_fac=='1')
i2 <- which(view_fac=='2')
i3 <- which(view_fac=='3')
i4 <- which(view_fac=='4')
n0 <- length(i0)
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n4 <- length(i4)
n  <- n0+n1+n2+n3+n4
g  <- length(levels(view_fac))

fit <- aov(LogY ~ view_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- LogY[permutation]
  fit_perm <- aov(LogY_perm ~ view_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor view has significantly effect on LogY


#TEST 1 vs 2

data1<-cbind(LogY[i1], view[i1])
data2<-cbind(LogY[i2], view[i2])
data12<-rbind(data1,data2)
data12<-as.data.frame(data12)
n<-length(data12[,1])
view_fac12 <- factor(data12$V2, levels=c('1','2'))
fit <- aov(data12$V1 ~ view_fac12)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm<- data12$V1[permutation]
  fit_perm <- aov(LogY_perm ~ view_fac12)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val  #0.603
#we don't reject H0
#Mean LogY of group 1 equal to mean LogY of group 2


#TEST 1 vs 3

data1<-cbind(LogY[i1], view[i1])
data3<-cbind(LogY[i3], view[i3])
data13<-rbind(data1,data3)
data13<-as.data.frame(data13)
n<-length(data13[,1])
view_fac13 <- factor(data13$V2, levels=c('1','3'))
fit <- aov(data13$V1 ~ view_fac13)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm<- data13$V1[permutation]
  fit_perm <- aov(LogY_perm ~ view_fac13)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val  #0
#we reject H0
#Mean LogY of group 1 different from mean LogY of group 3 with confidence greater than 99%

#TEST 2 vs 3

data2<-cbind(LogY[i2], view[i2])
data3<-cbind(LogY[i3], view[i3])
data23<-rbind(data2,data3)
data23<-as.data.frame(data23)
n<-length(data23[,1])
view_fac23 <- factor(data23$V2, levels=c('2','3'))
fit <- aov(data23$V1 ~ view_fac23)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm<- data23$V1[permutation]
  fit_perm <- aov(LogY_perm ~ view_fac23)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val  #0
#we reject H0
#Mean LogY of group 2 different from mean LogY of group 3 with confidence greater than 99%


#repeating the test for all the other pairs of groups, p-value 0 is always obtained
#therefore average LogY different among all other groups

# In conclusion, the average LogY of the houses that were given a medium-low evaluation (1) 
#of the visit and an average evaluation (2) of the visit is the same, and is in turn different 
#from the average logY of the houses to which it was given a low rating (0) and 
#a high rating (3.4) of visits, which are also different from each other


############ Verify effect of condition on LogY ########################

x11()
boxplot(LogY ~ condition, col=rainbow(8),main='Effect of condition on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

condition_fac <- factor(condition, levels=c('1','2','3','4','5'))
i1 <- which(condition_fac=='1')
i2 <- which(condition_fac=='2')
i3 <- which(condition_fac=='3')
i4 <- which(condition_fac=='4')
i5 <- which(condition_fac=='5')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n4 <- length(i4)
n5 <- length(i5)
n  <- n1+n2+n3+n4+n5
g  <- length(levels(condition_fac))

fit <- aov(LogY ~ condition_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- LogY[permutation]
  fit_perm <- aov(LogY_perm ~ condition_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor condition has significantly effect on LogY


#TEST 1 vs 2
data1<-cbind(LogY[i1], condition[i1])
data2<-cbind(LogY[i2], condition[i2])
data12<-rbind(data1,data2)
data12<-as.data.frame(data12)
n<-length(data12[,1])
condition_fac12 <- factor(data12$V2, levels=c('1','2'))
fit <- aov(data12$V1 ~ condition_fac12)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm<- data12$V1[permutation]
  fit_perm <- aov(LogY_perm ~ condition_fac12)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val  #0.261
#we don't reject H0
#Average LogY of group 1 equal to average LogY of group 2 


#TEST 3 vs 4
data3<-cbind(LogY[i3], condition[i3])
data4<-cbind(LogY[i4], condition[i4])
data34<-rbind(data3,data4)
data34<-as.data.frame(data34)
n<-length(data34[,1])
condition_fac34 <- factor(data34$V2, levels=c('3','4'))
fit <- aov(data34$V1 ~ condition_fac34)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm<- data34$V1[permutation]
  fit_perm <- aov(LogY_perm ~ condition_fac34)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val  #0
#we reject H0
#Mean LogY of group 3 different from mean LogY of group 4 with confidence greater than 99%

#repeating the test for all the other pairs of groups we obtain p-value 0 (sometimes 0.001) 
#therefore different average LogY among all the other groups

#In conclusion, the average LogY of houses in bad (1) and in bad (2) conditions is the same, 
#and is in turn different from the average logY of houses in normal (3), good (4) and 
#excellent (5) conditions. , which are also different from each other


################# Verify effect of has_bas on LogY #########################

x11()
boxplot(LogY ~ has_bas, col=rainbow(8),main='Effect of has_bas on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

has_bas_fac <- factor(has_bas, levels=c('0','1'))
i1 <- which(has_bas_fac=='1')
i2 <- which(has_bas_fac=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(has_bas_fac))

fit <- aov(LogY ~ has_bas_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- LogY[permutation]
  fit_perm <- aov(LogY_perm ~ has_bas_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor has_bas has significantly effect on LogY


############# Verify effect of grade on LogY ###################################

grade_fac <- factor(grade, levels=c('3','4','5','6','7','8','9','10','11','12','13'))
i3 <- which(grade_fac=='3')
i4 <- which(grade_fac=='4')
i5 <- which(grade_fac=='5')
i6 <- which(grade_fac=='6')
i7 <- which(grade_fac=='7')
i8 <- which(grade_fac=='8')
i9 <- which(grade_fac=='9')
i10 <- which(grade_fac=='10')
i11 <- which(grade_fac=='11')
i12 <- which(grade_fac=='12')
i13 <- which(grade_fac=='13')
n3 <- length(i3)
n4 <- length(i4)
n5 <- length(i5)
n6 <- length(i6)
n7 <- length(i7)
n8 <- length(i8)
n9 <- length(i9)
n10 <- length(i10)
n11 <- length(i11)
n12 <- length(i12)
n13 <- length(i13)
n <- n3+n4+n5+n6+n7+n8+n9+n10+n11+n12+n13
new_grade <- matrix(nrow=n,ncol=1)
D_new <- cbind(D,new_grade)
D_new[i3,36] <- 1
D_new[i4,36] <- 1
D_new[i5,36] <- 1
D_new[i6,36] <- 2
D_new[i7,36] <- 2
D_new[i8,36] <- 3
D_new[i9,36] <- 3
D_new[i10,36] <- 3
D_new[i11,36] <- 4
D_new[i12,36] <- 4
D_new[i13,36] <- 4

x11()
boxplot(D_new$log10.price. ~ D_new$new_grade, col=rainbow(8),main='Effect of new_grade on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

new_grade_fac <- factor(D_new$new_grade, levels=c('1','2','3','4'))
i1 <- which(new_grade_fac=='1')
i2 <- which(new_grade_fac=='2')
i3 <- which(new_grade_fac=='3')
i4 <- which(new_grade_fac=='4')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n4 <- length(i4)
n  <- n1+n2+n3+n4
g  <- length(levels(new_grade_fac))

fit <- aov(D_new$log10.price. ~ new_grade_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D_new$log10.price.[permutation]
  fit_perm <- aov(LogY_perm ~ new_grade_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor new_grade has significantly effect on LogY

#by carrying out the test for all the pairs of groups, p-value obtained is always 0, therefore the average LogY is different between all the groups of new_grade

################ Verify effect of 'Distance from the most southern point of Lake Union' on LogY ########################### --> Case sul Lake Union sono vicine a Seattle, capoluogo della Contea di King

i1 <- which(geodist_index < 15)
i2 <- which(geodist_index >= 15 & geodist_index <= 30)
i3 <- which(geodist_index > 30)
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3
dist <- matrix(nrow=n,ncol=1)
D_new <- cbind(D,dist)
D_new[i1,36] <- 1
D_new[i2,36] <- 2
D_new[i3,36] <- 3

x11()
boxplot(D_new$log10.price. ~ D_new$dist, col=rainbow(8),main='Effect of dist on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

dist_fac <- factor(D_new$dist, levels=c('1','2','3'))
i1 <- which(dist_fac=='1')
i2 <- which(dist_fac=='2')
i3 <- which(dist_fac=='3')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
g  <- length(levels(dist_fac))

fit <- aov(D_new$log10.price. ~ dist_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D_new$log10.price.[permutation]
  fit_perm <- aov(LogY_perm ~ dist_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor dist has significantly effect on LogY

#by carrying out the test for all the pairs of groups, p-value obtained is always 0, 
#therefore the average LogY is different between all the groups of dist


############## Verify effect of Log_sqm_living on LogY ################################

i1 <- which(Log_sqm_living < 2)   #which(sqm_living < 100)
i2 <- which(Log_sqm_living >= 2 & Log_sqm_living <= 2.4)  #which(sqm_living >= 100 & sqm_living <= 250)
i3 <- which(Log_sqm_living > 2.4)   #which(sqm_living > 250)
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3
sqm <- matrix(nrow=n,ncol=1)
D_new <- cbind(D,sqm)
D_new[i1,36] <- 1
D_new[i2,36] <- 2
D_new[i3,36] <- 3

x11()
boxplot(D_new$log10.price. ~ D_new$sqm, col=rainbow(8),main='Effect of Log_sqm_living on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

sqm_fac <- factor(D_new$sqm, levels=c('1','2','3'))
i1 <- which(sqm_fac=='1')
i2 <- which(sqm_fac=='2')
i3 <- which(sqm_fac=='3')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
g  <- length(levels(sqm_fac))

fit <- aov(D_new$log10.price. ~ sqm_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 10000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D_new$log10.price.[permutation]
  fit_perm <- aov(LogY_perm ~ sqm_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor sqm has significantly effect on LogY

#by carrying out the test for all the pairs of groups, p-value obtained is always 0, 
#therefore the average LogY is different between all the groups of sqm


####################### Verify effect of Log_sqm_living15 on LogY ################################################

i1 <- which(Log_sqm_living15 < 2)   #which(sqm_living15 < 100)
i2 <- which(Log_sqm_living15 >= 2 & Log_sqm_living15 <= 2.4)  #which(sqm_living15 >= 100 & sqm_living15 <= 250)
i3 <- which(Log_sqm_living15 > 2.4)   #which(sqm_living15 > 250)
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n <- n1+n2+n3
sqm15 <- matrix(nrow=n,ncol=1)
D_new <- cbind(D,sqm15)
D_new[i1,36] <- 1
D_new[i2,36] <- 2
D_new[i3,36] <- 3

x11()
boxplot(D_new$log10.price. ~ D_new$sqm15, col=rainbow(8),main='Effect of Log_sqm_living15 on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

sqm15_fac <- factor(D_new$sqm15, levels=c('1','2','3'))
i1 <- which(sqm15_fac=='1')
i2 <- which(sqm15_fac=='2')
i3 <- which(sqm15_fac=='3')
n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
g  <- length(levels(sqm15_fac))

fit <- aov(D_new$log10.price. ~ sqm15_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D_new$log10.price.[permutation]
  fit_perm <- aov(LogY_perm ~ sqm15_fac)
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

x11()
hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)
x11()
plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)
p_val <- sum(T_stat>=T0)/B
p_val #0
#we reject H0
#factor sqm15 has significantly effect on LogY

#by carrying out the test for all the pairs of groups, p-value obtained is always 0, 
#therefore the average LogY is different between all the groups of sqm15

#We could have figured this out as we found in a previous test that Log_sqm_living behaves 
#like Log_sqm_living15 so if the LogY of the small, medium and large sized houses is different,
#then the LogY of the houses with small, medium and large sized neighbors is also different

detach(D)
df_clean=D
attach(df_clean)

####################Verify relation between condition and view########################
#1)
## Goal: to understand if low condition implies low view
range(condition) # numeric varable between 1 and 5
range(view) # numeric varable between 1 and 5
viewtest=view+1 #same range variables

set.seed(240279)
B <- 10000

t1=cbind(condition)
t2=cbind(viewtest)
p=1
n1=length(t1)
n2=n1
n=n1+n2
## Test: H0: condition=view  vs H1:condition!=view

t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)
delta.0 <- 0
diff <- t1-t2
diff.mean <- colMeans(diff)

T20 <- as.numeric((diff.mean-delta.0)  %*% (diff.mean-delta.0))

T2 <- numeric(B)


for(perm in 1:B)
{
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

x11()
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)
x11()
plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)
p_val <- sum(T2>=T20)/B
p_val #0

# We refuse H0
# We can see that house condition and view has no correlation

###################################Test about richness####################################################

#Goal: To verify if the richer neighborhoods correspond to those where the houses cost more


i1=which(df_clean$is_rich==0)  
i2=which(df_clean$is_rich==1) 
t1=cbind(df_clean$price[i1]) #not rich neighborhoods
t2=cbind(df_clean$price[i2]) #rich neighborhoods
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n  <- n1 + n2
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)

#Test: H0:price houses of not rich neighborhoods=price houses of rich neighborhood vs H1=H0^c

T20 <- as.numeric((t1.mean-t2.mean) %*% (t1.mean-t2.mean))
T20

B <- 10000
T2 <- numeric(B)

for(perm in 1:B){
  # Random permutation of indexes
  t_pooled <- rbind(t1,t2)
  permutation <- sample(n)
  t_perm <- t_pooled[permutation]
  t1_perm <-cbind(t_perm[1:n1])
  t2_perm <- cbind(t_perm[(n1+1):n])
  
  # Evaluation of the test statistic on permuted data
  t1.mean_perm <- colMeans(t1_perm)
  t2.mean_perm <- colMeans(t2_perm)
  T2[perm]  <- (t1.mean_perm-t2.mean_perm) %*% (t1.mean_perm-t2.mean_perm) 
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=1000)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val    #pvalue=0


### => the price of houses in wealthy neighborhoods is much higher than that 
###    of houses in poor neighborhoods


####################Correlation between geodist_index and log10price################

B=1000
plot(df_clean$geodist_index,df_clean$`log10(price)`)

plot(df_clean$geodist_index,df_clean$`log10(price)`)
#Already from the graph one could guess that the closer we are to the center (lower geodist),
#the higher the prices will be

#dataset division in 3 parts based on distance  (short,medium,long)
i1=which(df_clean$geodist_index<10)
i2=which(df_clean$geodist_index>10 & df_clean$geodist_index<20)
i3=which(df_clean$geodist_index>=20)

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
v=vector(mode = "logical", length = n)
v[i1]="short"
v[i2]="medium"
v[i3]="long"
v=as.factor(v)  #creation of the categorical variable that indicates the distance
df_dis=cbind(df_clean,v)
colnames(df_dis)[36]="distance"
g <- nlevels(df_dis$distance)
plot(df_dis$distance, df_dis$`log10(price)`, xlab='treat',col=rainbow(g),main='Original Data')

fit <- aov(df_dis$`log10(price)` ~ df_dis$distance)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  log10price_perm <- df_dis$`log10(price)`[permutation]
  fit_perm <- aov(log10price_perm ~ df_dis$distance)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val  #0

# There is a significant correlation between price and geodist_index

