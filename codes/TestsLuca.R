DATA3<-read.csv("kc_house_data_clean.csv", header=TRUE, sep=",")
n <- dim(DATA3)[1]
attach(DATA3)

#CREATION OF CLEANED DATASET
Y = price
LogY = log10(price)
Log_sqm_living = log10(sqm_living)
Log_sqm_lot = log10(sqm_lot) 
Log_sqm_living15 = log10(sqm_living15) 
Log_sqm_lot15 = log10(sqm_lot15) 
DATA3 <- DATA
drop <- c("id","date","price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","yr_built","yr_renovated","zipcode","lat","long","sqft_living15","sqft_lot15")
DATA3 = DATA3[,!(names(DATA3) %in% drop)]
DATA3 <-  cbind(DATA3,Y,LogY,bedrooms,bathrooms,floors,bedfloors_ratio,bathfloors_ratio,sqm_above,sqm_basement,sqm_living,Log_sqm_living,sqm_lot,Log_sqm_lot,sqm_living15,Log_sqm_living15,sqm_lot15,Log_sqm_lot15,yr_built,yr_renovated,renov_index,geodist_index,ordinal_date,waterfront,view,condition,grade)


#TESTING
#Gaussian LogY test

B<-1000
shapiro.pvalue<-numeric(B)
for (b in 1:B){
  Yobs<-sample(D$LogY,5000)
  shapiro.pvalue[b]<-shapiro.test(Yobs)$p.value
}
sum(shapiro.pvalue>=0.1)/B    
#refuse normality of LogY


#H0: C(D$Log_sqm_living) == C(D$Log_sqm_living15) vs H1: C(D$Log_sqm_living) != C(D$Log_sqm_living15)

x11()
par(mfrow=c(1,3))
plot(D$Log_sqm_living,main='occurencies in DATA') 
hist(D$Log_sqm_living,main = 'distribution of occurencies',prob=T)
boxplot(D$Log_sqm_living)

x11()
par(mfrow=c(1,3))
plot(D$Log_sqm_living15,main='occurencies in DATA') 
hist(D$Log_sqm_living15,main = 'distribution of occurencies',prob=T)
boxplot(D$Log_sqm_living15)

t1 <- cbind(D$Log_sqm_living)
t2 <- cbind(D$Log_sqm_living15)
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
p_val #0.628
#we don't reject H0
#I can argue that the Log_sqm_living is equal to the Log_sqm_living15
#quindi le dimensioni della casa venduta sono uguali alle dimensioni medie delle case dei 15 vicini pi? vicini,
#per es. la casa venduta ? in un insieme di villette a schiera o in un insieme di ville, non una villa con casette piccoline vicine


#H0: D_OBBR$LogY =d= D_RBNR$LogY) vs H1: D_OBBR$LogY =!d!= D_RBBR$LogY

D_OBBR <- D[which(D$yr_built <= 1990 & D$yr_renovated != 0),]
D_RBNR <- D[which(D$yr_built > 1990 & D$yr_renovated == 0),]

T0<-abs(mean(D_OBBR$LogY)-mean(D_RBNR$LogY))
DataPooled<-c(D_OBBR$LogY,D_RBNR$LogY)
n<-length(DataPooled)
B<-10000
tstat<-numeric(B)
for(perm in 1:B){
  permutation<-sample(1:n)
  x_perm<-DataPooled[permutation]
  x1_perm<-x_perm[1:length(D_OBBR$LogY)]
  x2_perm<-x_perm[(length(D_RBNR$LogY)+1):n]
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


# verify effect of waterfront on LogY

x11()
boxplot(D$LogY ~ D$waterfront, col=rainbow(8),main='Effect of waterfront on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

watfront <- factor(D$waterfront, levels=c('0','1'))
i1 <- which(watfront=='1')
i2 <- which(watfront=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(watfront))

fit <- aov(D$LogY ~ watfront)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
  fit_perm <- aov(LogY_perm ~ watfront)
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


#