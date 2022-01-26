################################################################################
####################      FUNCTIONAL  DATA  CREATION      ######################
####################                   &                  ######################
####################       TIME   SERIES   ANALYSIS       ######################
################################################################################
kc_cleaned = read.csv("kc_cleaned.csv")
attach(kc_cleaned)

n = dim(kc_cleaned)[1]
ndays = 396

price_sqm_ratio = price / sqm_living

################################################################################
###################    DAILY price/sqm variation      ##########################
################################################################################
counter = numeric(ndays)
mean_daily_price_sqm_ratio = numeric(ndays)

for (i in 1:n){
  counter[ord_date[i]] = counter[ord_date[i]] +1
  mean_daily_price_sqm_ratio[ord_date[i]] = mean_daily_price_sqm_ratio[ord_date[i]] + price_sqm_ratio[i]
}
mean_daily_price_sqm_ratio = mean_daily_price_sqm_ratio/counter

week_days = rep(1,ndays)
for (i in 0:56){
  week_days[3+7*i] = 0
  week_days[4+7*i] = 0
}

plot(1:ndays,mean_daily_price_sqm_ratio, col = ifelse(week_days,"lightblue","red"),pch=20)

plot(1:length(mean_daily_price_sqm_ratio[week_days==1]),mean_daily_price_sqm_ratio[week_days==1],pch =16,col ="lightblue")

################################################################################
######################   WEEKLY price/sqm variation ####################################
################################################################################
nweeks = 57
week = numeric(n)

for (i in 1:n)
  week[i] = (ord_date[i]+2)%/%7+1

counter_weeks = numeric(nweeks)
price_sqm_ratio_weekly_mean = numeric(nweeks)

for (i in 1:n){
  counter_weeks[week[i]] = counter_weeks[week[i]] +1
  price_sqm_ratio_weekly_mean[week[i]] = price_sqm_ratio_weekly_mean[week[i]] + price_sqm_ratio[i]
}
price_sqm_ratio_weekly_mean = price_sqm_ratio_weekly_mean/counter_weeks

mean(price_sqm_ratio_weekly_mean) # 2855.558

plot(1:nweeks,price_sqm_ratio_weekly_mean,pch=20)   # NB: da levare le ultime 2 (guardare counter_weeks)
plot(1:(nweeks-2),price_sqm_ratio_weekly_mean[1:(nweeks-2)],pch=20)

################################################################################
#################       MONTHLY price/sqm variation     ########################
################################################################################
nmonths = 13
counter_months = numeric(nmonths)
mean_monthly_price_sqm_ratio = numeric(nmonths)

month = numeric(n)
for (i in 1:n) {
  if(ord_date[i]<=31)
    month[i] = 1
  else if(ord_date[i]<=61)
    month[i] = 2
  else if(ord_date[i]<=92)
    month[i] = 3
  else if(ord_date[i]<=123)
    month[i] = 4
  else if(ord_date[i]<=153)
    month[i] = 5
  else if(ord_date[i]<=184)
    month[i] = 6
  else if(ord_date[i]<=214)
    month[i] = 7
  else if(ord_date[i]<=245)
    month[i] = 8
  else if(ord_date[i]<=276)
    month[i] = 9
  else if(ord_date[i]<=304)
    month[i] = 10
  else if(ord_date[i]<=335)
    month[i] = 11
  else if(ord_date[i]<=365)
    month[i] = 12
  else if(ord_date[i]<=396)
    month[i] = 13
}

for (i in 1:n){
  counter_months[month[i]] = counter_months[month[i]]+1
  mean_monthly_price_sqm_ratio[month[i]] = mean_monthly_price_sqm_ratio[month[i]] + price_sqm_ratio[i]
}

mean_monthly_price_sqm_ratio = mean_monthly_price_sqm_ratio/counter_months

mean(mean_monthly_price_sqm_ratio)  #2835.131

plot(1:nmonths,mean_monthly_price_sqm_ratio,pch=20)

################################################################################
########################  Variation    PLOTs    #################################
################################################################################

x11()
par(mfrow=c(3,1))
plot(1:ndays,mean_daily_price_sqm_ratio, col = ifelse(week_days==1,"red","lightblue")
     ,pch=16, xlab = "days", ylab = "$/(m^2)")
title("DAILY")
plot(1:(nweeks-2),price_sqm_ratio_weekly_mean[-c(56,57)],pch=16, xlab = "weeks", ylab = "$/(m^2)")
title("WEEKLY")
plot(1:nmonths,mean_monthly_price_sqm_ratio,pch=16, xlab = "months", ylab = "$/(m^2)")  
title("MONTHLY")

################################################################################
############   Weekly price for sqm for each zip    ############################
################################################################################

# Finding the average of the houses sold in each zip.

zipcode = factor(zipcode)
nzips = length(levels(zipcode))
long_lat_means = matrix(NA,nrow = nzips, ncol = 2)

for (i in 1:nzips)
  long_lat_means[i,] = colMeans(cbind(long,lat)[which(zipcode==levels(zipcode)[i]),])

dimnames(long_lat_means)[[1]] = levels(zipcode)
dimnames(long_lat_means)[[2]] = c("Long","Lat")

# Chosing the appropriate number of clusters

b = NULL
w = NULL
for(k in 1:10){
  result.k = kmeans(long_lat_means, k)
  w = c(w, sum(result.k$wit))
  b = c(b, result.k$bet) }
x11()
matplot (1:10, w/(w+b), pch='', xlab='#clusters', ylab='within/tot', main='Choice of k', ylim=c(0,1))
lines(1:10, w/(w+b), type='b', lwd=2) 

k = 4
clust = kmeans(long_lat_means, centers = k ,nstart=10)
colors = c("red","blue","lightgreen","orange")
plot(long_lat_means,col = ifelse(clust$cluster==1,colors[1],
                          ifelse(clust$cluster==2,colors[2],
                          ifelse(clust$cluster==3,colors[3],colors[4]))),pch =16)

# given the within-total variability ratio, we choose k = 4.

# Computing the median price for each zip during each week.
zip_clust = numeric(n)
for (i in 1:n){
  found = FALSE
  j = 1
  while (!found){
    if (zipcode[i] == levels(zipcode)[j]){
      zip_clust[i] = result.k$cluster[j]
      found = TRUE
    }
    j = j + 1
  }
}

price_sqm_weekly_zip_mean = matrix(NA,nrow = nweeks,ncol = k)
for (i in 1:nweeks)
  for (j in 1:k)
    price_sqm_weekly_zip_mean[i,j] = mean(price_sqm_ratio[which(week == i & zip_clust == j)])
  
price_sqm_weekly_zip_median = matrix(NA,nrow = nweeks,ncol = k)
for (i in 1:nweeks)
  for (j in 1:k)
    price_sqm_weekly_zip_median[i,j] = median(price_sqm_ratio[which(week == i & zip_clust == j)])

# average variation for each cluster
plot(NA,NA,xlim = c(0,56),ylim=c(1500,4500),xlab = "Weeks",ylab="Price/sqm",main ="Mean variation")
lines(1:55,price_sqm_weekly_zip_mean[-c(56,57),1],col = colors[1])
lines(1:55,price_sqm_weekly_zip_mean[-c(56,57),2],col = colors[2])
lines(1:55,price_sqm_weekly_zip_mean[-c(56,57),3],col = colors[3])
lines(1:55,price_sqm_weekly_zip_mean[-c(56,57),4],col = colors[4])

# median variation for each cluster
plot(NA,NA,xlim = c(0,56),ylim=c(1500,4500),xlab = "Weeks",ylab="Price/sqm",main ="Median variation")
lines(1:55,price_sqm_weekly_zip_median[-c(56,57),1],col = colors[1],lwd=2)
lines(1:55,price_sqm_weekly_zip_median[-c(56,57),2],col = colors[2],lwd=2)
lines(1:55,price_sqm_weekly_zip_median[-c(56,57),3],col = colors[3],lwd=2)
lines(1:55,price_sqm_weekly_zip_median[-c(56,57),4],col = colors[4],lwd=2)

################################################################################
############   Monthly price for sqm for each zip    ###########################
################################################################################

zip = numeric(n)
for (i in 1:n) {
  found = FALSE
  z = 1
  while(!found){
    if(zipcode[i]==levels(zipcode)[z]){
      zip[i] = z
      found = TRUE
    }
    z =z+1
  }
}

psr_month_zip_mean = matrix(0, nrow = nmonths, ncol = nzips)
psr_month_zip_median = matrix(0, nrow = nmonths, ncol = nzips)

for (m in 1:nmonths)
  for(z in 1:nzips){
    psr_month_zip_mean[m,z] = mean(price_sqm_ratio[which(month==m & zip == z)])
    psr_month_zip_median[m,z] = median(price_sqm_ratio[which(month==m & zip == z)])
  }

which(psr_month_zip_mean[,] == 'NaN')
which(psr_month_zip_mean[-13,] == 'NaN')

# let's discard last month and correct in an easy way the missing values

psr_month_zip_mean[10,15] = (psr_month_zip_mean[9,15]+psr_month_zip_mean[11,15])/2
psr_month_zip_mean[8,25] = (psr_month_zip_mean[7,15]+psr_month_zip_mean[10,25])/3
psr_month_zip_mean[9,25] = (psr_month_zip_mean[7,15]+2*psr_month_zip_mean[10,25])/3

psr_month_zip_median[10,15] = (psr_month_zip_median[9,15]+psr_month_zip_median[11,15])/2
psr_month_zip_median[8,25] = (psr_month_zip_median[7,15]+psr_month_zip_median[10,25])/3
psr_month_zip_median[9,25] = (psr_month_zip_median[7,15]+2*psr_month_zip_median[10,25])/3


# monthly average for each zip:
plot(NA,NA,xlim = c(0,13),ylim=c(1500,7000),xlab = "Months",ylab="Price/sqm",main ="Mean variation")
matlines(psr_month_zip_mean[-13,which(clust$cluster==1)],col = colors[1])
matlines(psr_month_zip_mean[-13,which(clust$cluster==2)],col = colors[2])
matlines(psr_month_zip_mean[-13,which(clust$cluster==3)],col = colors[3])
matlines(psr_month_zip_mean[-13,which(clust$cluster==4)],col = colors[4])

# monthly median for each zip:
plot(NA,NA,xlim = c(0,13),ylim=c(1500,7000),xlab = "Months",ylab="Price/sqm",main ="Median variation")
matlines(psr_month_zip_median[-13,which(clust$cluster==1)],col = colors[1])
matlines(psr_month_zip_median[-13,which(clust$cluster==2)],col = colors[2])
matlines(psr_month_zip_median[-13,which(clust$cluster==3)],col = colors[3])
matlines(psr_month_zip_median[-13,which(clust$cluster==4)],col = colors[4])

# test mean-mean zip
p_val_mean = numeric()
B=1000
for(i in 1:3)
  for(j in (i+1):4){
    tot = rbind(t(psr_month_zip_mean[-13,which(clust$cluster==i)]),t(psr_month_zip_mean[-13,which(clust$cluster==j)]))
    n_1=clust$size[i]
    n_2=clust$size[j]
    nr = nrow(tot)
    meandiff=(colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==i)]))-colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==j)])))
    T0=sum(meandiff^2)
    T0_perm=numeric(B)
    for(perm in 1:B){
      permutazione <- sample(nr)
      tot_perm=tot[permutazione,]
      perm_1 = tot_perm[1:n_1,] 
      perm_2 = tot_perm[(n_1+1):nr,] 
      T0_perm[perm]=sum(((colMeans(perm_1)-colMeans(perm_2)))^2)
    }
    p_val_mean = c(p_val_mean,sum(T0_perm >= T0)/B)
  }

# no statistical evidence for saying SW-NE are different!

# test median-median zip
plot(NA,NA,xlim = c(0,13),ylim=c(1500,4500),xlab = "Months",ylab="Price/sqm",main ="Mean variation")
lines(1:12,colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==1)])),col = colors[1],lwd = 2)
lines(1:12,colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==2)])),col = colors[2],lwd = 2)
lines(1:12,colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==3)])),col = colors[3],lwd = 2)
lines(1:12,colMeans(t(psr_month_zip_mean[-13,which(clust$cluster==4)])),col = colors[4],lwd = 2)

p_val_median = numeric()
B=1000
for(i in 1:3)
  for(j in (i+1):4){
    tot = rbind(t(psr_month_zip_median[-13,which(clust$cluster==i)]),t(psr_month_zip_median[-13,which(clust$cluster==j)]))
    n_1=clust$size[i]
    n_2=clust$size[j]
    nr = nrow(tot)
    med_1 = numeric(12)
    med_2 = numeric(12)
    for (k in 1:12){
      med_1[k] = median(tot[n_1,k])
      med_2[k] = median(tot[n_2,k])
    }
    T0=sum(abs(med_1-med_2))
    T0_perm=numeric(B)
    for(perm in 1:B){
      permutazione <- sample(nr)
      tot_perm=tot[permutazione,]
      perm_1 = tot_perm[1:n_1,] 
      perm_2 = tot_perm[(n_1+1):nr,]
      med_1 = numeric(12)
      med_2 = numeric(12)
      for (k in 1:12){
        med_1[k] = median(perm_1[,k])
        med_2[k] = median(perm_2[,k])
      }
      T0_perm[perm]=sum(abs(med_1-med_2))
    }
    p_val_median = c(p_val_median,sum(T0_perm >= T0)/B)
  }

med_1 = numeric(12)
med_2 = numeric(12)
med_3 = numeric(12)
med_4 = numeric(12)
for (k in 1:12){
  med_1[k] = median(psr_month_zip_median[k,which(clust$cluster==1)])
  med_2[k] = median(psr_month_zip_median[k,which(clust$cluster==2)])
  med_3[k] = median(psr_month_zip_median[k,which(clust$cluster==3)])
  med_4[k] = median(psr_month_zip_median[k,which(clust$cluster==4)])
}

#no statistical evidence for saying: SW-NW, but also NW-SE and NE-SW, are different.

# test median-mean zip
p_val_medman = numeric()
B=1000
for(i in 1:3)
  for(j in (i+1):4){
    tot = rbind(t(psr_month_zip_median[-13,which(clust$cluster==i)]),t(psr_month_zip_median[-13,which(clust$cluster==j)]))
    n_1=clust$size[i]
    n_2=clust$size[j]
    nr = nrow(tot)
    meandiff=(colMeans(t(psr_month_zip_median[-13,which(clust$cluster==i)]))-colMedians(t(psr_month_zip_mean[-13,which(clust$cluster==j)])))
    T0=sum(meandiff^2)
    T0_perm=numeric(B)
    for(perm in 1:B){
      permutazione <- sample(nr)
      tot_perm=tot[permutazione,]
      perm_1 = tot_perm[1:n_1,] 
      perm_2 = tot_perm[(n_1+1):nr,] 
      T0_perm[perm]=sum(((colMeans(perm_1)-colMeans(perm_2)))^2)
    }
    p_val_medman = c(p_val_mean,sum(T0_perm >= T0)/B)
  }

#no statistical evidence for saying: SW-NW are different!

plot(NA,NA,xlim = c(0,13),ylim=c(1500,4500),xlab = "Months",ylab="Price/sqm",main ="Median variation")
lines(1:12,med_1,col = colors[1],lwd = 2)
lines(1:12,med_2,col = colors[2],lwd = 2)
lines(1:12,med_3,col = colors[3],lwd = 2)
lines(1:12,med_4,col = colors[4],lwd = 2)
