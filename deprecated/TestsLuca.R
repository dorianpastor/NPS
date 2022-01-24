#CLEANED DATASET

DATA$sqm_living = round(sqft_living*sqft_to_sqm)
DATA$sqm_lot = round(sqft_lot*sqft_to_sqm)
DATA$sqm_above = round(sqft_above*sqft_to_sqm)
DATA$sqm_basement = round(sqft_basement*sqft_to_sqm)
DATA$sqm_living15 = round(sqft_living15*sqft_to_sqm)
DATA$sqm_lot15 = round(sqft_lot15*sqft_to_sqm)
DATA$date = left_date
# Add
DATA$yr_old = 2015-yr_built
DATA$renovate_index =  c(ifelse(yr_renovated==0,DATA$yr_old,2015-yr_renovated))
DATA$has_ren = as.integer(as.logical(yr_renovated!=0))  # If it was renovated
DATA$has_bas = as.integer(as.logical(sqft_basement!=0))  # If it has basement, since many don't have it
DATA$ord_date = ordinal_date  # To take into account the market evolution
DATA$Y = price
LogY = log10(price)
Log_sqm_living = log10(sqm_living)
Log_sqm_lot = log10(sqm_lot) 
Log_sqm_living15 = log10(sqm_living15) 
Log_sqm_lot15 = log10(sqm_lot15) 
DATA <- cbind(DATA,LogY,bedfloors_ratio,bathfloors_ratio,Log_sqm_living,Log_sqm_lot,Log_sqm_living15,Log_sqm_lot15,geodist_index)
# Delete
DATA = subset(DATA, select=-c(id,date,price,zipcode,lat,long,sqft_above,sqft_basement,sqft_living,sqft_living15,sqft_lot,sqft_lot15)) # Drop useless columns

coll_columns = rbind(c("bedrooms","LogY"),c("bathrooms","LogY"),
                     c("bathfloors_ratio","LogY"),c("Log_sqm_living","LogY"),
                     c("Log_sqm_lot","LogY"),c("Log_sqm_living15","LogY"),
                     c("geodist_index","LogY"))
conta = 0  # Here I will count how many outliers we have
indexes = c()  # Here I will save all the indexes to discard
for (j in 1:dim(coll_columns)[1]){  # Iterate over all column-couples                
  columns = coll_columns[j,]
  print(columns)
  x11()
  bag = bagplot(DATA[columns])
  out = distinct(data.frame(bag$pxy.outlier))  # distinct to remove duplicates
  for (i in 1:dim(out)[1]){  # it can be optimized: loop to search for the outlier in the whole dataset
    row = out[i,]
    get_idx = which(DATA[columns[1]]==as.numeric(row["x"]) & DATA[columns[2]]==as.numeric(row["y"]))  # get the indexes where the outlier is found
    count_indexes = length(indexes)
    indexes = union(indexes,get_idx)
    conta= conta + length(indexes) - count_indexes
  }
  print(conta)
}
conta
indexes

DATA_CLEAN <- DATA[-indexes,]

#Verifico di aver rimosso tutti gli outliers
#provo il bagplot sul dataset pulito rimuovendo gli ouliers delle singole coppie di covariate una alla volta con il metodo nel ciclo di Enrico 
#e con il metodo del prof scritto di seguito e vediamo che combaciano 
#N.B. E' normale che questi bagplot siano diversi dal bagplot sul dataset pulito rimuovendo gli outliers di tutte le coppie di covariate insieme con il ciclo di Enrico perchè rimuovendo gli outliers
#     di tutte le coppie di covariate, dei punti che non erano outliers per una singola coppia di covariate, potrebbero ora diventarlo
#N.B. Non tutti i punti rossi fuori dalla figura del bagplot sono outliers 

D <- DATA_CLEAN 


#TESTING

#Gaussian LogY test

B<-1000
shapiro.pvalue<-numeric(B)
for (b in 1:B){
  Yobs<-sample(D$LogY,5000)
  shapiro.pvalue[b]<-shapiro.test(Yobs)$p.value
}
sum(shapiro.pvalue>=0.1)/B  #0 
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
p_val #0.627
#we don't reject H0
#I can argue that the Log_sqm_living is equal to the Log_sqm_living15
#quindi le dimensioni della casa venduta sono uguali alle dimensioni medie delle case dei 15 vicini più vicini,
#per es. la casa venduta è in un insieme di villette a schiera o in un insieme di ville, non una villa con casette piccoline vicine


#H0: D_OBBR$LogY =d= D_RBNR$LogY vs H1: D_OBBR$LogY =!d!= D_RBBR$LogY

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


#Verify effect of waterfront on LogY

x11()
boxplot(D$LogY ~ D$waterfront, col=rainbow(8),main='Effect of waterfront on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

waterfront_fac <- factor(D$waterfront, levels=c('0','1'))
i1 <- which(waterfront_fac=='1')
i2 <- which(waterfront_fac=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(waterfront_fac))

fit <- aov(D$LogY ~ waterfront_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
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


#Verify effect of view on LogY

x11()
boxplot(D$LogY ~ D$view, col=rainbow(8),main='Effect of view on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

view_fac <- factor(D$view, levels=c('0','1','2','3','4'))
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

fit <- aov(D$LogY ~ view_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
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

data1<-cbind(D$LogY[i1], D$view[i1])
data2<-cbind(D$LogY[i2], D$view[i2])
data12<-rbind(data1,data2)
data12<-as.data.frame(data12)
n<-length(data12[,1])
view_fac12 <- factor(data12$V2, levels=c('1','2'))
fit <- aov(data12$V1 ~ view_fac12)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
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
#LogY medio del gruppo 1 uguale a LogY medio del gruppo 2


#TEST 1 vs 3

data1<-cbind(D$LogY[i1], D$view[i1])
data3<-cbind(D$LogY[i3], D$view[i3])
data13<-rbind(data1,data3)
data13<-as.data.frame(data13)
n<-length(data13[,1])
view_fac13 <- factor(data13$V2, levels=c('1','3'))
fit <- aov(data13$V1 ~ view_fac13)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
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
#LogY medio del gruppo 1 diverso da LogY medio del gruppo 3 con confidenza maggiore di 99%


#TEST 2 vs 3

data2<-cbind(D$LogY[i2], D$view[i2])
data3<-cbind(D$LogY[i3], D$view[i3])
data23<-rbind(data2,data3)
data23<-as.data.frame(data23)
n<-length(data23[,1])
view_fac23 <- factor(data23$V2, levels=c('2','3'))
fit <- aov(data23$V1 ~ view_fac23)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
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
#LogY medio del gruppo 2 diverso da LogY medio del gruppo 3 con confidenza maggiore di 99%


#ripetendo il test per tutte le altre coppie di gruppi si ottiene sempre p-value 0
#quindi LogY medio diverso tra tutti gli altri gruppi

#In conclusione il LogY medio delle case a cui è stata data una medio-bassa valutazione (1) della visita e una media valutazione (2) della visita è uguale, ed è a sua volta diverso
#dai logY medi delle case a cui è stata data una bassa valuazione (0) e un'alta valutazione (3,4) delle visite, che sono diversi anche tra loro


#Verify effect of condition on LogY

x11()
boxplot(D$LogY ~ D$condition, col=rainbow(8),main='Effect of condition on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

condition_fac <- factor(D$condition, levels=c('1','2','3','4','5'))
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

fit <- aov(D$LogY ~ condition_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
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
data1<-cbind(D$LogY[i1], D$condition[i1])
data2<-cbind(D$LogY[i2], D$condition[i2])
data12<-rbind(data1,data2)
data12<-as.data.frame(data12)
n<-length(data12[,1])
condition_fac12 <- factor(data12$V2, levels=c('1','2'))
fit <- aov(data12$V1 ~ condition_fac12)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
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
#LogY medio del gruppo 1 uguale a LogY medio del gruppo 2


#TEST 3 vs 4
data3<-cbind(D$LogY[i3], D$condition[i3])
data4<-cbind(D$LogY[i4], D$condition[i4])
data34<-rbind(data3,data4)
data34<-as.data.frame(data34)
n<-length(data34[,1])
condition_fac34 <- factor(data34$V2, levels=c('3','4'))
fit <- aov(data34$V1 ~ condition_fac34)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
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
#LogY medio del gruppo 3 diverso da LogY medio del gruppo 4 con confidenza maggiore di 99%


#ripetendo il test per tutte le altre coppie di gruppi si ottiene p-value 0 (a volte 0.001)
#quindi LogY medio diverso tra tutti gli altri gruppi

#In conclusione il LogY medio delle case in pessime condizioni (1) e in cattive condizioni (2) è uguale, ed è a sua volta diverso
#dai logY medi delle case in condizioni normali (3), buone (4) e ottime (5), che sono diversi anche tra loro


#Verify effect of has_bas on LogY

x11()
boxplot(D$LogY ~ D$has_bas, col=rainbow(8),main='Effect of has_bas on LogY')
#hypothesys of normality in groups dissatisfied: permutational anova

has_bas_fac <- factor(D$has_bas, levels=c('0','1'))
i1 <- which(has_bas_fac=='1')
i2 <- which(has_bas_fac=='0')
n1 <- length(i1)
n2 <- length(i2)
n  <- n1+n2
g  <- length(levels(has_bas_fac))

fit <- aov(D$LogY ~ has_bas_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D$LogY[permutation]
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


#Verify effect of grade on LogY

grade_fac <- factor(D$grade, levels=c('3','4','5','6','7','8','9','10','11','12','13'))
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
D_new[i3,30] <- 1
D_new[i4,30] <- 1
D_new[i5,30] <- 1
D_new[i6,30] <- 2
D_new[i7,30] <- 2
D_new[i8,30] <- 3
D_new[i9,30] <- 3
D_new[i10,30] <- 3
D_new[i11,30] <- 4
D_new[i12,30] <- 4
D_new[i13,30] <- 4

x11()
boxplot(D_new$LogY ~ D_new$new_grade, col=rainbow(8),main='Effect of new_grade on LogY')
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

fit <- aov(D_new$LogY ~ new_grade_fac)
summary(fit)

T0 <- summary(fit)[[1]][1,4]
T0

B <- 1000 
T_stat <- numeric(B) 
for(perm in 1:B){
  permutation <- sample(1:n)
  LogY_perm <- D_new$LogY[permutation]
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


#effettuando il test per tutte le coppie di gruppi si ottiene sempre p-value 0
#quindi LogY medio diverso tra tutti i gruppi di new_grade
