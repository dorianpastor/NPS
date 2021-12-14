house<- read.csv("~/GitHub/lab_nonparametricstatistics/Block II - Nonparametric Inference/house_data.txt", sep="")
head(house)
summary(house)
dim(house)

B = 100000
seed = 26111992

house_ = house[c(3:16,20:21,26:33,35:39)]


#IDEA: permutational test per valutare price (considero la radice), sqm_living 
#e sqm_lot tra e 5k case pi? vecchie e le 5k più nuove o più recentemente rinnovate

cleaning=house_
cleaning=tibble::rowid_to_column(cleaning, "ID")
# t2 rappresenta le case più nuove
t2=NULL
for (i in 1:5000) {
  a=which.min(cleaning$yr_old)
  t2=rbind(t2,c(cleaning[which.min(cleaning$yr_old),]))
  id=cleaning$ID[which.min(cleaning$yr_old)]
  cleaning=cleaning[which(cleaning$ID!=id),]
}
# t1 rappresenta le case più vecchie
t1=NULL
for (i in 1:5000) {
  a=which.max(cleaning$yr_old)
  t1=rbind(t1,c(cleaning[which.max(cleaning$yr_old),]))
  id=cleaning$ID[which.max(cleaning$yr_old)]
  cleaning=cleaning[which(cleaning$ID!=id),]
}


t1=t1[,c(2,18,20)]
t2=t2[,c(2,18,20)]

t1=as.data.frame(t1)
t2=as.data.frame(t2)

t1$price=as.numeric(t1$price)
t1$sqm_living=as.numeric(t1$sqm_living)
t1$sqm_lot=as.numeric(t1$sqm_lot)
t2$price=as.numeric(t2$price)
t2$sqm_living=as.numeric(t2$sqm_living)
t2$sqm_lot=as.numeric(t2$sqm_lot)

#sostituisco il prezzo con la sua radice
t1$price=log10(t1$price)
t2$price=log10(t2$price)

library(rgl)


p  <- dim(t1)[2]
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n <- n1+n2

t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)
t1.cov  <-  cov(t1)
t2.cov  <-  cov(t2)
Sp      <- ((n1-1)*t1.cov + (n2-1)*t2.cov)/(n1+n2-2)
Spinv   <- solve(Sp)

delta.0 <- c(0,0,0)

diff <- t1-t2
diff.mean <- colMeans(diff)
diff.cov <- cov(diff)
diff.invcov <- solve(diff.cov)

#start with euclidian distance
T20 <- as.numeric((diff.mean-delta.0)  %*% (diff.mean-delta.0))

T2 <- numeric(B)
set.seed(seed)
for(perm in 1:B)
{
  # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% (diff.mean_perm-delta.0))
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val  #0.0023

# try malhanobis distance
T20 <- as.numeric( (diff.mean-delta.0) %*% solve(diag(diag(diff.cov))) %*% (diff.mean-delta.0))
# Estimating the permutational distribution under H0
T2 <- numeric(B)
set.seed(seed)
for(perm in 1:B)
{
  # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  
  
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% solve(diag(diag(diff.cov_perm))) %*% (diff.mean_perm-delta.0))
  
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

plot(ecdf(T2))
abline(v=T20,col=3,lwd=4)

# p-value
p_val <- sum(T2>=T20)/B
p_val  #0

#proper Mahalanobis distance
T20 <- as.numeric((diff.mean-delta.0) %*% diff.invcov %*% (diff.mean-delta.0))



# Estimating the permutational distribution under H0

set.seed(seed)
T2 <- numeric(B)

for(perm in 1:B)
{
  # Random permutation
  # obs: exchanging data within couples means changing the sign of the difference
  signs.perm <- rbinom(n1, 1, 0.5)*2 - 1
  
  diff_perm <- diff * matrix(signs.perm,nrow=n1,ncol=p,byrow=FALSE)
  diff.mean_perm <- colMeans(diff_perm)
  diff.cov_perm <- cov(diff_perm)
  diff.invcov_perm <- solve(diff.cov_perm)
  
  T2[perm] <- as.numeric((diff.mean_perm-delta.0) %*% diff.invcov_perm %*% (diff.mean_perm-delta.0))
}

# plotting the permutational distribution under H0
hist(T2,xlim=range(c(T2,T20)),breaks=100)
abline(v=T20,col=3,lwd=4)

p_val <- sum(T2>=T20)/B
p_val #0


# TEST TRA LOGPREZZO E GEODIST

attach(kc_cleaned)
plot(kc_cleaned$geodist_index,kc_cleaned$`log10(price)`)
#già dal grafico si potrebbe intuire che più ci troviamo vicini al centro, più i prezzi saranno alti

#Divido dataset in 3 parti in base alla distanza (breve,media,lunga)
i1=which(kc_cleaned$geodist_index<10)
i2=which(kc_cleaned$geodist_index>10 & kc_cleaned$geodist_index<20)
i3=which(kc_cleaned$geodist_index>=20)

n1 <- length(i1)
n2 <- length(i2)
n3 <- length(i3)
n  <- n1+n2+n3
v=vector(mode = "logical", length = n)
v[i1]="short"
v[i2]="medium"
v[i3]="long"
v=as.factor(v)
new_kc_cleaned=cbind(kc_cleaned,v)
colnames(new_kc_cleaned)[35]="distance"
g <- nlevels(new_kc_cleaned$distance)
plot(new_kc_cleaned$distance, new_kc_cleaned$`log10(price)`, xlab='treat',col=rainbow(g),main='Original Data')

fit <- aov(new_kc_cleaned$`log10(price)` ~ new_kc_cleaned$distance)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

T_stat <- numeric(B) 

for(perm in 1:B){
  # Permutation:
  permutation <- sample(1:n)
  log10price_perm <- new_kc_cleaned$`log10(price)`[permutation]
  fit_perm <- aov(log10price_perm ~ new_kc_cleaned$distance)
  
  # Test statistic:
  T_stat[perm] <- summary(fit_perm)[[1]][1,4]
}

hist(T_stat,xlim=range(c(T_stat,T0)),breaks=30)
abline(v=T0,col=3,lwd=2)

plot(ecdf(T_stat),xlim=c(-1,20))
abline(v=T0,col=3,lwd=4)

p_val <- sum(T_stat>=T0)/B
p_val  #0


# TEST TRA