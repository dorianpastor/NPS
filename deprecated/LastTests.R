#Lavoro sul dataset df_clean (ovvero quello ottenuto dopo la rimozione degli outlier nel
# preprocessing)

head(df_clean)
attach(df_clean)


#1)
# Obiettivo: capire se bassa condition implica bassa view
range(condition) # variabile numerica che va da 1 a 5
range(view) #variabile numerica che va da 0 a 4
viewtest=view+1 #variabili con stesso range

set.seed(240279)
B <- 10000

x11()
par(mfrow=c(1,3))
plot(condition,main='Houses conditions between 1 and 5') 
hist(condition,prob=T)
boxplot(condition)

x11()
par(mfrow=c(1,3))
plot(viewtest,main='Houses views between 1 and 5') 
hist(viewtest,prob=T)
boxplot(viewtest)


t1=cbind(condition)
t2=cbind(viewtest)
p=1
n1=length(t1)
n2=n1
n=n1+n2
## Test: H0: condition=view  vs H1:condition!=view
# Uso test alle differenze
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

# Rifiutiamo H0
# Possiamo notare che le condizioni della casa non hanno alcuna relazione con
# la sua visibilità


#2)
# Obiettivo: Verificare se i quartieri più ricchi corrispondono a quelli dove le
# case costano di più

i1=which(df_clean$is_rich==0)  
i2=which(df_clean$is_rich==1) 
t1=cbind(df_clean$price[i1]) #quartieri non ricchi
t2=cbind(df_clean$price[i2]) #quartieri ricchi
n1 <- dim(t1)[1]
n2 <- dim(t2)[1]
n  <- n1 + n2
t1.mean <- colMeans(t1)
t2.mean <- colMeans(t2)

#Test:    H0:prezzo case quartieri non ricchi=prezzo case quartieri ricchi vs H1=H0^c

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

### => il prezzo delle case nei quartieri ricchi è molto più alto di quello 
###    delle case nei quartieri non ricchi



#3)
#Obiettivo: permutational test per valutare price (considero log10(price)), sqm_living 
#e sqm_lot tra le 5k case piu vecchie e le 5k più nuove o più recentemente rinnovate


df_ID=tibble::rowid_to_column(df_clean, "ID")
# t2 rappresenta le case più nuove
t2=NULL
for (i in 1:5000) {
  a=which.min(df_ID$yr_old)
  t2=rbind(t2,c(df_ID[which.min(df_ID$yr_old),]))
  id=df_ID$ID[which.min(df_ID$yr_old)]
  df_ID=df_ID[which(df_ID$ID!=id),]
}

# t1 rappresenta le case più vecchie
t1=NULL
for (i in 1:5000) {
  a=which.max(df_ID$yr_old)
  t1=rbind(t1,c(df_ID[which.max(df_ID$yr_old),]))
  id=df_ID$ID[which.max(df_ID$yr_old)]
  df_ID=df_ID[which(df_ID$ID!=id),]
}
t1=t1[,c(29,17,18)]
t2=t2[,c(29,17,18)]
t1=data.frame(t1)
t2=data.frame(t2)
colnames(t1)=c("log10(price","sqm_living","sqm_lot")
colnames(t2)=c("log10(price","sqm_living","sqm_lot")
t1$log10.price=as.numeric(t1$log10.price)
t1$sqm_living=as.numeric(t1$sqm_living)
t1$sqm_lot=as.numeric(t1$sqm_lot)
t2$log10.price=as.numeric(t2$log10.price)
t2$sqm_living=as.numeric(t2$sqm_living)
t2$sqm_lot=as.numeric(t2$sqm_lot)

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
# Possiamo concludere che, per un livello di confidenza maggiore del 99%, le case
# più vecchie e quelle più nuove presentano una significativa differenza tra le
# 3 variabili considerate


# Try malhanobis distance
T20 <- as.numeric( (diff.mean-delta.0) %*% solve(diag(diag(diff.cov))) %*% (diff.mean-delta.0))
# Estimating the permutational distribution under H0
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

#Stesse conclusioni tratte in precedenza


#proper Mahalanobis distance
T20 <- as.numeric((diff.mean-delta.0) %*% diff.invcov %*% (diff.mean-delta.0))

# Estimating the permutational distribution under H0

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

# medesime conclusioni con tutte e 3 le distanze testate




#4)
# Obiettivo: valutare se c'è correlazione tra le variabili calcolate geodist e logprezzo
# TEST TRA LOGPREZZO E GEODIST
B=1000
attach(df_clean)
plot(df_clean$geodist_index,df_clean$`log10(price)`)
# Già dal grafico si potrebbe intuire che più ci troviamo vicini al centro (geodist minore),
# più i prezzi saranno alti

#Divido dataset in 3 parti in base alla distanza (breve,media,lunga)
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
v=as.factor(v)  #creazione variabile categorica che indica la distanza
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

# C'è un legame significativo tra prezzo e geodist_index





#Obiettivo: 
# variabile isrich da plottare sulla mappa con lo zipcode 

library(sf)
library(mapview)
data=cbind(lat,long)
locations <- as_tibble(data)
locations_sf <- st_as_sf(locations, coords = c("lat", "long"), crs = 4326)
mapview(locations_sf)

